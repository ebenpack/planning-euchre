{-# LANGUAGE ScopedTypeVariables #-}
module WsApp where

import qualified Control.Concurrent  as Concurrent
import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import qualified Control.Monad.Loops as Loops
import qualified Data.Map            as Map
import qualified Data.Text           as Text
import qualified Network.WebSockets  as WS
import qualified Types               as T

disconnectClient :: WS.Connection -> Concurrent.MVar T.App -> T.UserId -> IO ()
disconnectClient conn state uid = Concurrent.modifyMVar state $ \s -> do
    let newState = T.removeUser s uid
    -- TODO: send stuff, destroy other stuff
    return (newState, ())


nextId :: Map.Map T.UserId T.User -> T.UserId
nextId = Map.foldrWithKey (\k _ b -> max (1 + k) b) 0


connectClient :: T.UserName -> Concurrent.MVar T.App -> IO T.UserId
connectClient uname state = Concurrent.modifyMVar state $ \s -> do
    let usrs = T.getUsers s
        uid = nextId usrs
        newState = T.addUser s T.User { T.userName=uname, T.userId=uid }
    return (newState, uid)


handleRequests :: WS.Connection -> Concurrent.MVar T.App -> IO a
handleRequests conn state = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    -- TODO: Parse command, update state, send stuff
    WS.sendTextData conn msg


getUserName :: WS.Connection -> IO T.UserName
getUserName conn = Loops.untilJust $ do
    msg :: Text.Text <- WS.receiveData conn
    case T.parseCommand msg of
        Just (T.Connect uname) -> return $ Just uname
        _                      -> return Nothing


wsApp :: Concurrent.MVar T.App -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    uname <- getUserName conn
    uid <- connectClient uname state
    Exception.finally
        (handleRequests conn state)
        (disconnectClient conn state uid)
