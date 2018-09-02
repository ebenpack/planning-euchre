{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.WsApp
    ( wsApp
    , State(State)
    , _app
    , _userState
    )
where

import qualified Control.Concurrent            as Concurrent
import qualified Control.Exception             as Exception
import           Control.Lens                   ( allOf
                                                , anyOf
                                                , at
                                                , sans
                                                , (%~)
                                                , (&)
                                                , (.~)
                                                , (?~)
                                                , (^.)
                                                , (^?)
                                                , _Just
                                                )
import           Control.Lens.Fold              ( folded )
import           Control.Lens.Traversal         ( traverse )
import           Control.Lens.Tuple             ( _2 )
import qualified Control.Monad                 as Monad
import qualified Control.Monad.Loops           as Loops
import           Data.Aeson                     ( encode )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.IntMap.Strict            as M
import           Data.Maybe                     ( isJust )
import qualified Data.Text                     as Text
import qualified Network.WebSockets            as WS

import qualified Common.App                    as A
                                                ( Users
                                                , rooms
                                                , users
                                                )
import           Common.Card                    ( Card )
import           Common.Command                 ( Command
                                                    ( Connect
                                                    , Connected
                                                    , CreateNewStory
                                                    , CreateRoom
                                                    , DestroyRoom
                                                    , JoinRoom
                                                    , LeaveRoom
                                                    , NewStoryCreated
                                                    , RoomCreated
                                                    , RoomDestroyed
                                                    , RoomJoined
                                                    , RoomLeft
                                                    , Vote
                                                    , VotingComplete
                                                    )
                                                , parseCommand
                                                )
import           Common.Deck                    ( Deck )
import           Common.Room                   as Room
                                                ( Private
                                                , Room(Room)
                                                , RoomId
                                                , RoomName
                                                , RoomState(Voting, Results)
                                                , _roomDeck
                                                , _roomId
                                                , _roomName
                                                , _roomOwner
                                                , _roomPrivate
                                                , _roomResult
                                                , _roomStory
                                                , _roomUsers
                                                , _roomState
                                                )
import           Common.Story                   ( Story )
import           Common.User                    ( User(User)
                                                , UserName
                                                , userId
                                                , _userId
                                                , _userName
                                                )
import           Backend.State                 as State
                                                ( UserState
                                                    ( UserState
                                                    , _connection
                                                    , _room
                                                    )
                                                , State(State, _app, _userState)
                                                , CommandHandler
                                                , room
                                                , app
                                                , userState
                                                , appRoomUser
                                                , appRoomUsers
                                                , appRoomStory
                                                , appRoomVotes
                                                , appRoomState
                                                , appRoom
                                                , appUser
                                                , userStateConnection
                                                , userStateRoom
                                                , appRoomVote
                                                , appRoomDeck
                                                , userInAnyRoom
                                                , userInRoom
                                                , userOwnsRoom
                                                )


nextId :: M.IntMap a -> Int
nextId m = head $ dropWhile (`M.member` m) [1 ..]


broadcast :: State -> A.Users -> ByteString -> IO ()
broadcast s users msg = Monad.forM_ users $ \u -> broadcastUser s u msg


broadcastUser :: State -> User -> ByteString -> IO ()
broadcastUser state user msg =
    let uid  = user ^. userId
        conn = state ^? userStateConnection uid
    in  case conn of
            Just conn' -> WS.sendTextData conn' msg
            _          -> return ()


broadcastApp :: State -> ByteString -> IO ()
broadcastApp state msg =
    let appUsers = state ^. app . A.users in broadcast state appUsers msg


broadcastRoom :: State -> RoomId -> ByteString -> IO ()
broadcastRoom state rid msg =
    let rmUsers = state ^. appRoomUsers rid
    in  broadcast state (M.map fst rmUsers) msg


disconnectClient :: Concurrent.MVar State -> User -> IO ()
disconnectClient state usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid      = usr ^. userId
        rm       = s ^? userStateRoom uid . _Just
        newState = s & app . A.users %~ sans uid & userState %~ sans uid

    case rm of
        Just rid | newState & userOwnsRoom uid rid ->
            destroyRoom rid newState usr
        Just rid -> leaveRoom rid newState usr
        _        -> return newState


connectClient :: UserName -> Concurrent.MVar State -> WS.Connection -> IO User
connectClient uname state conn = Concurrent.modifyMVar state $ \s -> do
    let usrs     = s ^. app . A.users
        uid      = nextId usrs
        usr      = User {_userName = uname, _userId = uid}
        msg      = encode $ Connected uid
        newState = s & appUser uid ?~ usr & userState . at uid ?~ UserState
            { _connection = conn
            , _room       = Nothing
            }
    broadcastUser newState usr msg
    return (newState, usr)


createRoom :: RoomName -> Story -> Deck -> Private -> CommandHandler
createRoom rname stry dck prvt s usr = do
    let rms           = s ^. app . A.rooms
        uid           = usr ^. userId
        userNotInRoom = not (s & userInAnyRoom uid)
        rid           = nextId rms
    -- TODO: Make user leave room?
    if userNotInRoom
        then do
            let rm = Room
                    { _roomId      = rid
                    , _roomName    = rname
                    , _roomOwner   = uid
                    , _roomUsers   = M.singleton uid (usr, Nothing)
                    , _roomStory   = stry
                    , _roomResult  = Nothing
                    , _roomDeck    = dck
                    , _roomPrivate = prvt
                    , _roomState   = Voting
                    }
                newState = s & userStateRoom uid ?~ rid & appRoom rid ?~ rm
            broadcastRoom newState rid $ encode $ RoomCreated rm
            return newState
        else return s


joinRoom :: RoomId -> CommandHandler
joinRoom rid s usr = do
    let uid           = usr ^. userId
        userNotInRoom = not (s & userInAnyRoom uid)
    let newState = if userNotInRoom
            then
                s
                &  appRoomUser rid uid
                ?~ (usr, Nothing)
                &  userStateRoom uid
                ?~ rid
            else s
    let room'' = newState ^? appRoom rid . _Just
    Monad.when userNotInRoom $ case room'' of
        Just r -> do
            let msg = encode $ RoomJoined r
            broadcastRoom newState rid msg
        Nothing -> return ()
    return newState


leaveRoom :: RoomId -> CommandHandler
leaveRoom rid s usr = do
    let uid      = usr ^. userId
        inRoom   = isJust $ s ^? appRoomUser rid uid
        ownsRoom = s & userOwnsRoom uid rid
    if ownsRoom
        then destroyRoom rid s usr
        else if inRoom
            then do
                let newState = s & appRoomUsers rid %~ sans uid
                    newRoom  = newState ^? appRoom rid . _Just
                case newRoom of
                    Just r  -> broadcastRoom newState rid $ encode $ RoomLeft r
                    Nothing -> return ()
                return newState
            else return s

destroyRoom :: RoomId -> CommandHandler
destroyRoom rid s usr = do
    let uid    = usr ^. userId
        ownsRm = s & userOwnsRoom uid rid
        rm     = s ^? appRoom rid . _Just
    case rm of
        Just rm' -> if ownsRm
            then do
                broadcastRoom s rid (encode $ RoomDestroyed rid)
                return
                    $  s
                    &  userState
                    .  traverse
                    .  room
                    .~ Nothing
                    &  app
                    .  A.rooms
                    %~ sans rid
            else do
                broadcastRoom s rid (encode $ RoomLeft rm')
                return $ s & appRoomUsers rid %~ sans uid
        Nothing -> return s


newStory :: RoomId -> Story -> CommandHandler
newStory rid stry s usr = do
    let uid      = usr ^. userId
        ownsRm   = s & userOwnsRoom uid rid
        newState = if ownsRm
            then
                s
                &  appRoomStory rid
                .~ stry
                &  appRoomVotes rid
                .~ Nothing
                &  appRoomState rid
                .~ Voting
            else s
        rm = newState ^? appRoom rid . _Just
    case rm of
        Just rm' -> broadcastRoom newState rid $ encode $ NewStoryCreated rm'
        _        -> return ()
    return newState

makeVote :: Card -> CommandHandler
makeVote crd s usr = do
    let uid = usr ^. userId
        rid = s ^? userStateRoom uid . _Just
    case rid of
        Nothing   -> return s
        Just rid' -> do
            let inRoom      = s & userInRoom uid rid'
                legalCard   = s & anyOf (appRoomDeck rid' . folded) (== crd)
                isLegalVote = inRoom && legalCard
                voting      = (s ^? appRoomState rid') == Just Voting
            if not isLegalVote && not voting
                then return s
                else do
                    let newState       = s & appRoomVote rid' uid ?~ crd
                        votingComplete = newState & allOf
                            (appRoomUsers rid' . folded . _2)
                            (\case
                                Just _ -> True
                                _      -> False
                            )
                        newState' = if votingComplete
                            then newState & appRoomState rid' .~ Results
                            else newState
                        rm = newState' ^? appRoom rid' . _Just

                    Monad.when votingComplete $ case rm of
                        Just rm' -> broadcastRoom
                            newState'
                            rid'
                            (encode $ VotingComplete rm')
                        _ -> return ()
                    return newState'


-- TODO: REMOVE
printState :: CommandHandler
printState s _ = do
    broadcastApp s $ encode $ s ^. app
    return s


handleCommand :: Command -> Concurrent.MVar State -> User -> IO ()
handleCommand cmd state usr = Concurrent.modifyMVar_ state $ \s -> case cmd of
    CreateRoom rname stry dck prvt -> createRoom rname stry dck prvt s usr
    DestroyRoom rid                -> destroyRoom rid s usr
    JoinRoom    rid                -> joinRoom rid s usr
    LeaveRoom   rid                -> leaveRoom rid s usr
    CreateNewStory rid stry        -> newStory rid stry s usr
    Vote crd                       -> makeVote crd s usr
    _                              -> printState s usr


handleRequests :: Concurrent.MVar State -> WS.Connection -> User -> IO a
handleRequests state conn usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    case parseCommand msg of
        Just c  -> handleCommand c state usr
        Nothing -> return ()


getUserName :: WS.Connection -> IO UserName
getUserName conn = Loops.untilJust $ do
    msg :: Text.Text <- WS.receiveData conn
    case parseCommand msg of
        Just (Connect uname) | not $ Text.null uname -> return $ Just uname
        _ -> return Nothing


wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    uname <- getUserName conn
    usr   <- connectClient uname state conn
    Exception.finally (handleRequests state conn usr)
                      (disconnectClient state usr)
