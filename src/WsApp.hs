{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module WsApp (wsApp, State(State), _app, _userState) where

import qualified App                    as A (App, rooms, users)
import           Command                (Command (Connect), parseCommand)
import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception      as Exception
import           Control.Lens           (at, makeLenses, sans, (%~), (&), (?~),
                                         (^.), (^?), _Just)

import           Control.Lens.Traversal (traverse)
import qualified Control.Monad          as Monad
import qualified Control.Monad.Loops    as Loops
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Network.WebSockets     as WS
import           Room                   (RoomId)
import           User                   (User (User), UserId, UserName, userId,
                                         _userId, _userName)

data UserState = UserState { _connection :: WS.Connection
                           , _rooms      :: [RoomId]}
data State = State { _app :: A.App, _userState :: Map.Map UserId UserState }

makeLenses ''UserState
makeLenses ''State

broadcast :: Concurrent.MVar State -> Text.Text -> IO ()
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
        newState = foldr (\r s -> s & (app . A.rooms) %~ sans r) s rms
            & (app . A.users) %~ sans uid
            & (userState %~ sans uid)
    -- TODO: send stuff, destroy other stuff
    return newState


nextId :: Map.Map UserId User -> UserId
nextId = Map.foldrWithKey (\k _ b -> max (1 + k) b) 0


connectClient :: UserName -> WS.Connection -> Concurrent.MVar State -> IO User
connectClient uname conn state = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid = nextId usrs
        usr = User { _userName=uname, _userId=uid }
        newState = s
            & (app . (A.users . at uid) ?~ usr)
            & (userState . at uid) ?~ UserState {_connection=conn, _rooms=[]}
    return (newState, usr)


handleRequests :: WS.Connection -> Concurrent.MVar State -> User -> IO a
handleRequests conn state usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    broadcast state msg


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
