{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module WsApp (wsApp, State(State), _app, _userState) where

import qualified App                    as A (App, Users, rooms, users)
import           Command                (Command (Connect, CreateRoom, DestroyRoom, JoinRoom, LeaveRoom, RoomDestroyed, RoomJoined, RoomLeft),
                                         parseCommand)
import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception      as Exception
import           Control.Lens           (anyOf, at, makeLenses, sans, (%~), (&),
                                         (.~), (?~), (^.), (^?), _Just)
import           Control.Lens.Fold      (folded)
import           Control.Lens.Traversal (traverse)
import qualified Control.Monad          as Monad
import qualified Control.Monad.Loops    as Loops
import           Data.Aeson             (encode)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.Map               as Map
import           Data.Maybe             (isJust)
import qualified Data.Text              as Text
import           Deck                   (Deck)
import qualified Network.WebSockets     as WS
import           Room                   (Private, Room (Room), RoomId, RoomName,
                                         roomOwner, roomUsers, _roomDeck,
                                         _roomId, _roomName, _roomOwner,
                                         _roomPrivate, _roomStory, _roomUsers)
import           Story                  (Story)
import           User                   (User (User), UserId, UserName, userId,
                                         _userId, _userName)

data UserState = UserState { _connection :: WS.Connection
                           , _room       :: Maybe RoomId}
data State = State { _app :: A.App, _userState :: Map.Map UserId UserState }

makeLenses ''UserState
makeLenses ''State

nextId :: Integral a => Map.Map a b -> a
nextId m = head $ dropWhile (`Map.member` m) [1..]


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


-- |Predicate function indicating whether a given user currently resides in some (any) room
userInRoom :: UserId -> State -> Bool
userInRoom uid = anyOf (app . A.rooms . folded . roomUsers) (\u -> case u ^. at uid of
    Just _ -> True
    _      -> False)

-- |Predicate function indicating whether a given user owns the room specified
userOwnsRoom :: UserId -> RoomId -> State -> Bool
userOwnsRoom uid rid = anyOf (app . A.rooms . at rid . _Just . roomOwner) (== uid)

disconnectClient :: Concurrent.MVar State -> WS.Connection -> User -> IO ()
disconnectClient state conn usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid = usr ^. userId
        rm = s ^? userState . at uid . _Just . room . _Just
        newState = s
            & app . A.users %~ sans uid
            & userState %~ sans uid

    case rm of
        Just rid ->
            if newState & userOwnsRoom uid rid then
                destroyRoom rid newState conn usr
            else
                leaveRoom rid newState conn usr
        _        -> return newState


connectClient :: UserName ->  Concurrent.MVar State -> WS.Connection -> IO User
connectClient uname state conn = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid = nextId usrs
        usr = User { _userName=uname, _userId=uid }
        newState = s
            & app . A.users . at uid ?~ usr
            & userState . at uid ?~ UserState {_connection=conn, _room=Nothing}
    return (newState, usr)


destroyRoom :: RoomId -> State -> WS.Connection -> User -> IO State
destroyRoom rid s _ usr = do
    let uid = usr ^. userId
        ownsRm = s & userOwnsRoom uid rid
        msg = encode $ RoomDestroyed rid
    Monad.when ownsRm $ broadcastRoom rid s msg
    return $ if ownsRm then
        s
            & (userState . traverse . room) .~ Nothing
            & app . A.rooms %~ sans rid
    else
        s


createRoom :: RoomName -> Story -> Deck -> Private -> State -> WS.Connection -> User -> IO State
createRoom rname stry dck prvt s _ usr = do
    let rms = s ^. app . A.rooms
        uid = usr ^. userId
    return $
        if not (s & userInRoom uid) then
            let rid = nextId rms
                rm = Room {
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
                    & userState . at uid . _Just . room ?~ rid
                    & app . A.rooms . at rid ?~ rm
        else
            s

joinRoom :: RoomId -> State -> WS.Connection -> User -> IO State
joinRoom rid s _ usr = do
    let uid = usr ^. userId
    if not (s & userInRoom uid) then do
        let newState = s
                & app . A.rooms . at rid . _Just . roomUsers . at uid ?~ usr
                & userState . at uid . _Just . room ?~ rid
            msg = encode $ RoomJoined rid usr
        broadcastRoom rid newState msg
        return newState
    else
        return s

leaveRoom :: RoomId -> State -> WS.Connection -> User -> IO State
leaveRoom rid s _ usr = do
    let uid = usr ^. userId
        inRoom = isJust $ s ^? app . A.rooms . at rid . _Just . roomUsers . at uid
    if inRoom then do
        let newState = s
                & app . A.rooms . at rid . _Just . roomUsers %~ sans uid
            msg = encode $ RoomLeft rid usr
        broadcastRoom rid newState msg
        return newState
    else
        return s

-- TODO: REMOVE
printState :: State -> WS.Connection -> User -> IO State
printState s _ _ = do
        broadcastApp s $ encode $ s ^. app
        return s


handleCommand :: Command -> Concurrent.MVar State -> WS.Connection -> User -> IO ()
handleCommand cmd state conn usr = Concurrent.modifyMVar_ state $ \s ->
    case cmd of
        CreateRoom rname stry dck prvt -> createRoom rname stry dck prvt s conn usr
        DestroyRoom rid                -> destroyRoom rid s conn usr
        JoinRoom rid                   -> joinRoom rid s conn usr
        LeaveRoom rid                  -> leaveRoom rid s conn usr
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
