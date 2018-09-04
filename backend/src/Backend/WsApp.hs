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

import           Backend.State                 as State
                                                ( CommandHandler
                                                , State(State, _app, _userState)
                                                , UserState
                                                    ( UserState
                                                    , _connection
                                                    , _room
                                                    )
                                                , app
                                                , appRoom
                                                , appRoomOwner
                                                , appRoomDeck
                                                , appRoomState
                                                , appRoomStory
                                                , appRoomUser
                                                , appRoomUsers
                                                , appRoomVote
                                                , appRoomVotes
                                                , appUser
                                                , room
                                                , userInAnyRoom
                                                , userInRoom
                                                , userOwnsRoom
                                                , userState
                                                , userStateConnection
                                                , userStateRoom
                                                )
import qualified Common.App                    as App
import           Common.Card                    ( Card )
import qualified Common.Command                as Command
import           Common.Deck                    ( Deck )
import           Common.Room                   as Room
                                                ( Private
                                                , Room(Room)
                                                , RoomId
                                                , RoomName
                                                , RoomState
                                                    ( VotingComplete
                                                    , VotingOpen
                                                    , VotingClosed
                                                    )
                                                , _roomDeck
                                                , _roomId
                                                , _roomName
                                                , _roomOwner
                                                , _roomPrivate
                                                , _roomState
                                                , _roomStory
                                                , _roomUsers
                                                )
import           Common.Story                   ( Story )
import           Common.User                    ( User(User)
                                                , UserId
                                                , UserName
                                                , userId
                                                , _userId
                                                , _userName
                                                )


nextId :: M.IntMap a -> M.Key
nextId m = head $ dropWhile (`M.member` m) [1 ..]


broadcast :: Foldable t => State -> t User -> ByteString -> IO ()
broadcast s usrs msg = Monad.forM_ usrs $ \u -> broadcastUser s u msg


broadcastUser :: State -> User -> ByteString -> IO ()
broadcastUser s usr msg =
    let uid  = usr ^. userId
        conn = s ^? userStateConnection uid
    in  case conn of
            Just conn' -> WS.sendTextData conn' msg
            _          -> return ()


broadcastUserById :: State -> UserId -> ByteString -> IO ()
broadcastUserById s uid msg =
    let conn = s ^? userStateConnection uid
    in  case conn of
            Just conn' -> WS.sendTextData conn' msg
            _          -> return ()


broadcastApp :: State -> ByteString -> IO ()
broadcastApp s msg =
    let appUsers = s ^. app . App.users in broadcast s appUsers msg


broadcastRoom :: State -> RoomId -> ByteString -> IO ()
broadcastRoom s rid msg =
    let rmUsers = s ^. appRoomUsers rid in broadcast s (M.map fst rmUsers) msg


disconnectClient :: Concurrent.MVar State -> User -> IO ()
disconnectClient s usr = Concurrent.modifyMVar_ s $ \s' -> do
    let uid      = usr ^. userId
        rid      = s' ^? userStateRoom uid . _Just
        newState = s' & app . App.users %~ sans uid & userState %~ sans uid

    case rid of
        Just rid' | newState & userOwnsRoom uid rid' ->
            destroyRoom rid' newState usr
        Just rid' -> leaveRoom rid' newState usr
        _         -> return newState


connectClient :: UserName -> Concurrent.MVar State -> WS.Connection -> IO User
connectClient nm s conn = Concurrent.modifyMVar s $ \s' -> do
    let usrs     = s' ^. app . App.users
        uid      = nextId usrs
        usr      = User {_userName = nm, _userId = uid}
        msg      = encode $ Command.Connected uid
        newState = s' & appUser uid ?~ usr & userState . at uid ?~ UserState
            { _connection = conn
            , _room       = Nothing
            }
    broadcastUser newState usr msg
    return (newState, usr)


createRoom :: RoomName -> Story -> Deck -> Private -> CommandHandler
createRoom rnm stry dck prvt s usr = do
    let rms       = s ^. app . App.rooms
        uid       = usr ^. userId
        usrInRoom = s & userInAnyRoom uid
        rid       = nextId rms
    -- TODO: Make user leave room?
    if usrInRoom
        then return s
        else do
            let rm = Room
                    { _roomId      = rid
                    , _roomName    = rnm
                    , _roomOwner   = uid
                    , _roomUsers   = M.singleton uid (usr, Nothing)
                    , _roomStory   = stry
                    , _roomDeck    = dck
                    , _roomPrivate = prvt
                    , _roomState   = VotingOpen
                    }
                newState = s & userStateRoom uid ?~ rid & appRoom rid ?~ rm
            broadcastRoom newState rid $ encode $ Command.RoomCreated rm
            return newState


joinRoom :: RoomId -> CommandHandler
joinRoom rid s usr = do
    let uid       = usr ^. userId
        usrInRoom = s & userInAnyRoom uid
    let
        newState = if usrInRoom
            then s
            else
                s
                &  appRoomUser rid uid
                ?~ (usr, Nothing)
                &  userStateRoom uid
                ?~ rid
    let rm = newState ^? appRoom rid . _Just
    Monad.unless usrInRoom $ case rm of
        Just r -> do
            let msg = encode $ Command.RoomJoined r
            broadcastRoom newState rid msg
        Nothing -> return ()
    return newState


leaveRoom :: RoomId -> CommandHandler
leaveRoom rid s usr = do
    let uid      = usr ^. userId
        inRoom   = s & userInRoom uid rid
        ownsRoom = s & userOwnsRoom uid rid
        rmOwner  = s ^? appRoomOwner rid
    if ownsRoom
        then destroyRoom rid s usr
        else if inRoom
            then do
                let newState =
                        s
                            &  appRoomUsers rid
                            %~ sans uid
                            &  userStateRoom uid
                            .~ Nothing
                    rm     = newState ^? appRoom rid . _Just
                    votingComplete =
                        newState & allOf (appRoomUsers rid . folded . _2) isJust
                    newState' = if votingComplete
                        then newState & appRoomState rid .~ VotingComplete
                        else newState
                case rm of
                    Just rm' ->
                        broadcastRoom newState' rid $ encode $ Command.RoomLeft
                            rm'
                    Nothing -> return ()
                case rmOwner of
                    Just rmOwner' ->
                        Monad.when votingComplete $ broadcastUserById
                            newState'
                            rmOwner'
                            (encode $ Command.VotingReadyToClose rid)
                    _ -> return ()
                return newState'
            else return s

destroyRoom :: RoomId -> CommandHandler
destroyRoom rid s usr = do
    let uid    = usr ^. userId
        ownsRm = s & userOwnsRoom uid rid
        rm     = s ^? appRoom rid . _Just
    case rm of
        Just _ | ownsRm -> do
            broadcastRoom s rid (encode $ Command.RoomDestroyed rid)
            return
                $  s
                &  app
                .  App.rooms
                %~ sans rid
                &  userState
                .  traverse
                .  room
                %~ (\case
                       Just r' | r' == rid -> Nothing
                       a                   -> a
                   )
        Just rm' -> do
            broadcastRoom s rid (encode $ Command.RoomLeft rm')
            return
                $  s
                &  appRoomUsers rid
                %~ sans uid
                &  userStateRoom uid
                .~ Nothing
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
                .~ VotingOpen
            else s
        rm = newState ^? appRoom rid . _Just
    case rm of
        Just rm' | ownsRm ->
            broadcastRoom newState rid $ encode $ Command.NewStoryCreated rm'
        _ -> return ()
    return newState

makeVote :: Card -> CommandHandler
makeVote crd s usr = do
    let uid = usr ^. userId
        rid = s ^? userStateRoom uid . _Just
    case rid of
        Just rid' -> do
            let inRoom      = s & userInRoom uid rid'
                isLegalCard = s & anyOf (appRoomDeck rid' . folded) (== crd)
                voting      = (s ^? appRoomState rid') == Just VotingOpen
                isLegalVote = inRoom && isLegalCard && voting
                rmOwner     = s ^? appRoomOwner rid'
            if not isLegalVote
                then return s
                else do
                    let newState = s & appRoomVote rid' uid ?~ crd
                        votingComplete =
                            newState
                                & allOf (appRoomUsers rid' . folded . _2) isJust
                        newState' = if votingComplete
                            then newState & appRoomState rid' .~ VotingComplete
                            else newState
                    case rmOwner of
                        Just rmOwner' ->
                            Monad.when votingComplete $ broadcastUserById
                                newState'
                                rmOwner'
                                (encode $ Command.VotingReadyToClose rid')
                        _ -> return ()
                    return newState'
        Nothing -> return s


closeVote :: RoomId -> CommandHandler
closeVote rid s usr = do
    let uid            = usr ^. userId
        ownsRm         = s & userOwnsRoom uid rid
        votingComplete = (s ^? appRoomState rid) == Just VotingComplete
    if ownsRm && votingComplete
        then do
            let newState = s & appRoomState rid .~ VotingClosed
                rm       = newState ^? appRoom rid . _Just
            case rm of
                Just rm' -> broadcastRoom
                    newState
                    rid
                    (encode $ Command.VotingClosed rm')
                _ -> return ()
            return newState
        else return s

-- TODO: REMOVE
printState :: CommandHandler
printState s _ = do
    broadcastApp s $ encode $ s ^. app
    return s


handleCommand :: Command.Command -> Concurrent.MVar State -> User -> IO ()
handleCommand cmd s usr = Concurrent.modifyMVar_ s $ \s' -> case cmd of
    Command.CreateRoom rnm stry dck prvt -> createRoom rnm stry dck prvt s' usr
    Command.DestroyRoom rid         -> destroyRoom rid s' usr
    Command.JoinRoom    rid         -> joinRoom rid s' usr
    Command.LeaveRoom   rid         -> leaveRoom rid s' usr
    Command.CreateNewStory rid stry -> newStory rid stry s' usr
    Command.Vote      crd           -> makeVote crd s' usr
    Command.CloseVote rid           -> closeVote rid s' usr
    _                               -> printState s' usr


handleRequests :: Concurrent.MVar State -> WS.Connection -> User -> IO a
handleRequests s conn usr = Monad.forever $ do
    msg :: Text.Text <- WS.receiveData conn
    case Command.parseCommand msg of
        Just c  -> handleCommand c s usr
        Nothing -> return ()


getUserName :: WS.Connection -> IO UserName
getUserName conn = Loops.untilJust $ do
    msg :: Text.Text <- WS.receiveData conn
    case Command.parseCommand msg of
        Just (Command.Connect unm) | not $ Text.null unm -> return $ Just unm
        _ -> return Nothing


wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp s pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    unm <- getUserName conn
    usr <- connectClient unm s conn
    Exception.finally (handleRequests s conn usr) (disconnectClient s usr)
