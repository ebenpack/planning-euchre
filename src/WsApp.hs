{-# LANGUAGE ScopedTypeVariables #-}
module WsApp (wsApp) where

import           App                 (App, addUser, getUsers, removeUser)
import           Command             (Command (Connect), parseCommand)
import qualified Control.Concurrent  as Concurrent
import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import qualified Control.Monad.Loops as Loops
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Network.WebSockets  as WS
import           User                (User (User), UserId, UserName, connection,
                                      userId, userName)

broadcast :: Concurrent.MVar App -> Text.Text -> IO ()
broadcast state msg = do
    s <- Concurrent.readMVar state
    let users = getUsers s
    Monad.forM_ users $ \c ->
        WS.sendTextData (connection c) msg

disconnectClient :: WS.Connection -> Concurrent.MVar App -> UserId -> IO ()
disconnectClient conn state uid = Concurrent.modifyMVar state $ \s -> do
    let newState = removeUser s uid
    -- TODO: send stuff, destroy other stuff
    return (newState, ())


nextId :: Map.Map UserId User -> UserId
nextId = Map.foldrWithKey (\k _ b -> max (1 + k) b) 0


connectClient :: UserName -> WS.Connection -> Concurrent.MVar App -> IO UserId
connectClient uname conn state = Concurrent.modifyMVar state $ \s -> do
    let usrs = getUsers s
        uid = nextId usrs
        newState = addUser s User { userName=uname, userId=uid, connection=conn }
    return (newState, uid)


handleRequests :: WS.Connection -> Concurrent.MVar App -> IO a
handleRequests conn state = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    broadcast state msg


getUserName :: WS.Connection -> IO UserName
getUserName conn = Loops.untilJust $ do
    msg :: Text.Text <- WS.receiveData conn
    case parseCommand msg of
        Just (Connect uname) -> return $ Just uname
        _                    -> return Nothing


wsApp :: Concurrent.MVar App -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    uname <- getUserName conn
    uid <- connectClient uname conn state
    Exception.finally
        (handleRequests conn state)
        (disconnectClient conn state uid)
