{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module WsApp (wsApp, State(State), _app, _userState) where

import qualified App                  as A (App, rooms, users)
import           Card                 (Card (Three))
import           Command              (Command (Connect, Connected, CreateRoom, DestroyRoom, Disconnect, Disconnected, JoinRoom, NewStory, Vote),
                                       parseCommand)
import qualified Control.Concurrent   as Concurrent
import qualified Control.Exception    as Exception
import           Control.Lens         (at, makeLenses, sans, (%~), (&), (?~),
                                       (^.), (^?), _Just)
import qualified Control.Monad        as Monad
import qualified Control.Monad.Loops  as Loops
import           Data.Aeson.Encode    (encode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map             as Map
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   (decodeUtf8)
import           Deck                 (Deck (Deck))
import qualified Network.WebSockets   as WS
import           Room                 (Private, Room (Room), RoomId, RoomName,
                                       _roomDeck, _roomId, _roomName,
                                       _roomOwner, _roomPrivate, _roomStory,
                                       _roomUsers)
import           Story                (Story)
import           User                 (User (User), UserId, UserName, userId,
                                       _userId, _userName)

data UserState = UserState { _connection :: WS.Connection
                           , _rooms      :: [RoomId]}
data State = State { _app :: A.App, _userState :: Map.Map UserId UserState }

makeLenses ''UserState
makeLenses ''State

broadcast :: Concurrent.MVar State -> ByteString -> IO ()
broadcast state msg = do
    s <- Concurrent.readMVar state
    let appUsers = s ^. app . A.users
    Monad.forM_ appUsers $ \u ->
        let uid = u ^. userId
            conn = s ^? userState . at uid . _Just . connection
        in
            case conn of
                Just conn' -> WS.sendTextData conn' msg
                _          -> return ()

disconnectClient :: WS.Connection -> Concurrent.MVar State -> User -> IO ()
disconnectClient conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid = usr ^. userId
        rms = s ^. userState . at uid . _Just . rooms
        newState = foldr (\r s' -> s' & app . A.rooms %~ sans r) s rms
            & app . A.users %~ sans uid
            & userState %~ sans uid
    -- TODO: send stuff, destroy other stuff
    return newState


nextId :: Integral a => Map.Map a b -> a
nextId m = if mn > 0 then mn - 1 else (mx + 1)
    where (mn, mx) = Map.foldrWithKey minmax  (0,0) m
          minmax k _ b = (max k (fst b), min k (snd b))


connectClient :: UserName -> WS.Connection -> Concurrent.MVar State -> IO User
connectClient uname conn state = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid = nextId usrs
        usr = User { _userName=uname, _userId=uid }
        newState = s
            & app . A.users . at uid ?~ usr
            & userState . at uid ?~ UserState {_connection=conn, _rooms=[]}
    return (newState, usr)


createRoom :: RoomName -> Story -> Deck -> Private -> WS.Connection -> Concurrent.MVar State -> User -> IO ()
createRoom rname stry dck prvt conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    -- check if user already in another room? already owns a room?
    let uid = usr ^. userId
        rms = s ^. app . A.rooms
        rid = nextId rms
        room = Room {
              _roomId=rid
            , _roomName=rname
            , _roomOwner=uid
            , _roomUsers=Map.empty
            , _roomStory=stry
            , _roomDeck=dck
            , _roomPrivate=prvt
        }
        newState = s
            & userState . at uid . _Just . rooms %~ (++[rid])
            & app . A.rooms . at uid ?~ room
    print $ show uid
    return newState


printState conn state usr = Concurrent.modifyMVar_ state $ \s -> do
    broadcast state $ encode $ s ^. app
    return s

handleCommand :: Command -> WS.Connection -> Concurrent.MVar State -> User -> IO ()
handleCommand cmd = case cmd of
        CreateRoom rname stry dck prvt -> createRoom rname stry dck prvt
        _                              -> printState


handleRequests :: WS.Connection -> Concurrent.MVar State -> User -> IO a
handleRequests conn state usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    case parseCommand msg of
        Just c  -> handleCommand c conn state usr
        Nothing -> return ()
    s <- Concurrent.readMVar state
    broadcast state $ encode $ s ^. app


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
