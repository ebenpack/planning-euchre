{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module WsApp (wsApp, State(State), _app, _userState) where

import qualified App                    as A (App, Users, rooms, users)
import           Card                   (Card (Three))
import           Command                (Command (Connect, Connected, CreateRoom, DestroyRoom, Disconnect, Disconnected, JoinRoom, NewStory, RoomDestroyed, Vote),
                                         parseCommand)
import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception      as Exception
import           Control.Lens           (anyOf, at, has, makeLenses, sans, (%~),
                                         (&), (?~), (^.), (^?), _Just)
import           Control.Lens.Fold      (folded)
import           Control.Lens.Traversal (traverse)
import qualified Control.Monad          as Monad
import qualified Control.Monad.Loops    as Loops
import           Data.Aeson.Encode      (encode)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     (decodeUtf8)
import           Deck                   (Deck (Deck))
import qualified Network.WebSockets     as WS
import           Room                   (Private, Room (Room), RoomId, RoomName,
                                         roomOwner, roomUsers, _roomDeck,
                                         _roomId, _roomName, _roomOwner,
                                         _roomPrivate, _roomStory, _roomUsers)
import           Story                  (Story)
import           User                   (User (User), UserId, UserName, userId,
                                         _userId, _userName)

data UserState = UserState { _connection :: WS.Connection
                           , _rooms      :: Set.Set RoomId}
data State = State { _app :: A.App, _userState :: Map.Map UserId UserState }

makeLenses ''UserState
makeLenses ''State

broadcast :: State -> A.Users -> ByteString -> IO ()
broadcast s users msg = do
    Monad.forM_ users $ \u ->
        let uid = u ^. userId
            conn = s ^? userState . at uid . _Just . connection
        in
            case conn of
                Just conn' -> WS.sendTextData conn' msg
                _          -> return ()

broadcastApp :: State -> ByteString -> IO ()
broadcastApp state msg = do
    let appUsers = state ^. app . A.users
    broadcast state appUsers msg

broadcastRoom :: RoomId -> State -> ByteString -> IO ()
broadcastRoom rid state msg = do
    let roomUsers = state ^. app . A.users
    broadcast state roomUsers msg

disconnectClient :: WS.Connection -> Concurrent.MVar State -> User -> IO ()
disconnectClient conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid = usr ^. userId
        rms = s ^. userState . at uid . _Just . rooms
        -- TODO: traversal?
        newState = foldr (\r s' -> s' & app . A.rooms %~ sans r) s rms
            & app . A.users %~ sans uid
            & userState %~ sans uid
    -- TODO: send stuff, destroy other stuff
    return newState


nextId :: Integral a => Map.Map a b -> a
nextId m = head $ dropWhile (`Map.member` m) [1..]

connectClient :: UserName -> WS.Connection -> Concurrent.MVar State -> IO User
connectClient uname conn state = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid = nextId usrs
        usr = User { _userName=uname, _userId=uid }
        newState = s
            & app . A.users . at uid ?~ usr
            & userState . at uid ?~ UserState {_connection=conn, _rooms=Set.empty}
    return (newState, usr)


destroyRoom :: RoomId -> WS.Connection -> Concurrent.MVar State -> User -> IO ()
destroyRoom rid conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid = usr ^. userId
        rmOwner = s ^? app . A.rooms . at rid . _Just . roomOwner
        userOwnsRoom = case rmOwner of
            Just rmOwnerId -> rmOwnerId == uid
            Nothing        -> False
        msg = encode $ RoomDestroyed rid
    Monad.when userOwnsRoom $ broadcastRoom rid s msg
    return $ if userOwnsRoom then
        s
            & (userState . traverse . rooms) %~ sans rid
            & app . A.rooms %~ sans rid
    else
        s

createRoom :: RoomName -> Story -> Deck -> Private -> WS.Connection -> Concurrent.MVar State -> User -> IO ()
createRoom rname stry dck prvt conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    let rms = s ^. app . A.rooms
        uid = usr ^. userId
        userInRoom = s & anyOf (app . A.rooms . folded . roomUsers) (\u -> case u ^. at uid of
            Just _ -> True
            _      -> False)
    return $
        if not userInRoom then
            let rid = nextId rms
                room = Room {
                    _roomId=rid
                    , _roomName=rname
                    , _roomOwner=uid
                    , _roomUsers=Map.singleton uid usr
                    , _roomStory=stry
                    , _roomDeck=dck
                    , _roomPrivate=prvt
                }
            in
                s
                    & userState . at uid . _Just . rooms %~ (Set.insert rid)
                    & app . A.rooms . at rid ?~ room
        else
            s


printState conn state usr = do
        s <- Concurrent.readMVar state
        broadcastApp s $ encode $ s ^. app
        return ()

handleCommand :: Command -> WS.Connection -> Concurrent.MVar State -> User -> IO ()
handleCommand cmd = case cmd of
        CreateRoom rname stry dck prvt -> createRoom rname stry dck prvt
        DestroyRoom rid                -> destroyRoom rid
        _                              -> printState


handleRequests :: WS.Connection -> Concurrent.MVar State -> User -> IO a
handleRequests conn state usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    case parseCommand msg of
        Just c  -> handleCommand c conn state usr
        Nothing -> return ()
    printState conn state usr


getUserName :: WS.Connection -> IO UserName
getUserName conn = Loops.untilJust $ do
    msg :: Text.Text <- WS.receiveData conn
    case parseCommand msg of
        Just (Connect uname) -> return $ Just uname
        _                    -> return Nothing


wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    uname <- getUserName conn
    usr <- connectClient uname conn state
    Exception.finally
        (handleRequests conn state usr)
        (disconnectClient conn state usr)
