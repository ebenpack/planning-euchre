{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module WsApp (wsApp, State(State), _app, _userState) where

import qualified App                    as A (App, Users, rooms, users)
import           Command                (Command (Connect, Connected, CreateRoom, DestroyRoom, Disconnect, Disconnected, JoinRoom, NewStory, RoomDestroyed, Vote),
                                         parseCommand)
import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception      as Exception
import           Control.Lens           (anyOf, at, makeLenses, sans, (%~), (&),
                                         (.~), (?~), (^.), (^?), _Just)
import           Control.Lens.Fold      (filtered, folded, (^..))
import           Control.Lens.Prism     (only)
import           Control.Lens.Traversal (traverse)
import qualified Control.Monad          as Monad
import qualified Control.Monad.Loops    as Loops
import           Data.Aeson             (encode)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     (decodeUtf8)
import           Deck                   (Deck)
import qualified Network.WebSockets     as WS
import           Room                   (Private, Room (Room), RoomId, RoomName,
                                         roomId, roomOwner, roomUsers,
                                         _roomDeck, _roomId, _roomName,
                                         _roomOwner, _roomPrivate, _roomStory,
                                         _roomUsers)
import           Story                  (Story)
import           User                   (User (User), UserId, UserName, userId,
                                         _userId, _userName)

data UserState = UserState { _connection :: WS.Connection
                           , _rooms      :: Set.Set RoomId}
data State = State { _app :: A.App, _userState :: Map.Map UserId UserState }

makeLenses ''UserState
makeLenses ''State

broadcast :: State -> A.Users -> ByteString -> IO ()
broadcast s users msg = Monad.forM_ users $ \u ->
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
    let rmUsers = state ^. app . A.rooms . at rid . _Just . roomUsers
    broadcast state rmUsers msg

disconnectClient :: Concurrent.MVar State -> WS.Connection -> User -> IO ()
disconnectClient state conn usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid = usr ^. userId
        rms = s ^. app . A.rooms
        destroy = Map.filterWithKey (\_ r -> (r ^. roomOwner) == uid) rms
        newState = s
            & app . A.users %~ sans uid
            & userState %~ sans uid
    newState' <- Monad.forM (Map.toList destroy) $ \(_, r) -> destroyRoom (r ^. roomId) newState conn usr
    return  $ foldl (curry snd) newState newState'


nextId :: Integral a => Map.Map a b -> a
nextId m = head $ dropWhile (`Map.member` m) [1..]

connectClient :: UserName ->  Concurrent.MVar State -> WS.Connection -> IO User
connectClient uname state conn = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid = nextId usrs
        usr = User { _userName=uname, _userId=uid }
        newState = s
            & app . A.users . at uid ?~ usr
            & userState . at uid ?~ UserState {_connection=conn, _rooms=Set.empty}
    return (newState, usr)


destroyRoom :: RoomId -> State -> WS.Connection -> User -> IO State
destroyRoom rid s conn usr = do
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

createRoom :: RoomName -> Story -> Deck -> Private -> State -> WS.Connection -> User -> IO State
createRoom rname stry dck prvt s conn usr = do
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


-- TODO: REMOVE
printState :: State -> WS.Connection -> User -> IO State
printState s conn usr = do
        broadcastApp s $ encode $ s ^. app
        return s

handleCommand :: Command -> Concurrent.MVar State -> WS.Connection -> User -> IO ()
handleCommand cmd state conn usr = Concurrent.modifyMVar_ state $ \s ->
    case cmd of
        CreateRoom rname stry dck prvt -> createRoom rname stry dck prvt s conn usr
        DestroyRoom rid                -> destroyRoom rid s conn usr
        _                              -> printState s conn usr


handleRequests :: Concurrent.MVar State -> WS.Connection ->  User -> IO a
handleRequests state conn usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    case parseCommand msg of
        Just c  -> handleCommand c state conn usr
        Nothing -> return ()
    s <- Concurrent.readMVar state
    printState s conn usr


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
    usr <- connectClient uname state conn
    Exception.finally
        (handleRequests state conn usr)
        (disconnectClient state conn usr)
