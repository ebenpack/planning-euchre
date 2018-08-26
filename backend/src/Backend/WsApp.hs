{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Backend.WsApp
    ( wsApp
    , State(State)
    , _app
    , _userState
    )
where

import qualified Control.Concurrent     as Concurrent
import qualified Control.Exception      as Exception
import           Control.Lens           (allOf, anyOf, at, makeLenses, sans,
                                         (%~), (&), (.~), (?~), (^.), (^..),
                                         (^?), _Just)
import           Control.Lens.Fold      (folded)
import           Control.Lens.Traversal (traverse)
import           Control.Lens.Tuple     (_1, _2)
import qualified Control.Monad          as Monad
import qualified Control.Monad.Loops    as Loops
import           Data.Aeson             (encode)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.IntMap.Strict     as M
import           Data.Maybe             (isJust)
import qualified Data.Text              as Text
import qualified Network.WebSockets     as WS

import qualified Common.App             as A (App, Users, rooms, users)
import           Common.Card            (Card)
import           Common.Command         (Command (Connect, Connected, CreateNewStory, CreateRoom, DestroyRoom, JoinRoom, LeaveRoom, NewStoryCreated, RoomCreated, RoomDestroyed, RoomJoined, RoomLeft, Vote, VotingComplete),
                                         parseCommand)
import           Common.Deck            (Deck)
import           Common.Room            (Private, Room (Room), RoomId, RoomName,
                                         roomDeck, roomOwner, roomStory,
                                         roomUsers, _roomDeck, _roomId,
                                         _roomName, _roomOwner, _roomPrivate,
                                         _roomResult, _roomStory, _roomUsers)
import           Common.Story           (Story)
import           Common.User            (User (User), UserId, UserName, userId,
                                         _userId, _userName)


data UserState = UserState { _connection :: WS.Connection
                           , _room       :: Maybe RoomId}

data State = State { _app :: A.App, _userState :: M.IntMap UserState }

type CommandHandler = State -> User -> IO State

makeLenses ''UserState
makeLenses ''State

nextId :: M.IntMap a -> Int
nextId m = head $ dropWhile (`M.member` m) [1 ..]


broadcast :: State -> A.Users -> ByteString -> IO ()
broadcast s users msg = Monad.forM_ users $ \u -> broadcastUser s u msg


broadcastUser :: State -> User -> ByteString -> IO ()
broadcastUser state user msg =
    let uid  = user ^. userId
        conn = state ^? userState . at uid . _Just . connection
    in  case conn of
            Just conn' -> WS.sendTextData conn' msg
            _          -> return ()


broadcastApp :: State -> ByteString -> IO ()
broadcastApp state msg = do
    let appUsers = state ^. app . A.users
    broadcast state appUsers msg


broadcastRoom :: RoomId -> State -> ByteString -> IO ()
broadcastRoom rid state msg = do
    let rmUsers = state ^. app . A.rooms . at rid . _Just . roomUsers
    broadcast state (M.map fst rmUsers) msg


-- |Predicate function indicating whether a given user currently resides in some (any) room
userInAnyRoom :: UserId -> State -> Bool
userInAnyRoom uid = anyOf
    (userState . at uid . folded)
    (\u -> case u ^. room of
        Just _ -> True
        _      -> False
    )


-- |Predicate function indicating whether a given user currently resides in some (any) room
userInRoom :: UserId -> RoomId -> State -> Bool
userInRoom uid rid = anyOf
    (app . A.rooms . at rid . _Just . roomUsers . folded . _1 . userId)
    (== uid)


-- |Predicate function indicating whether a given user owns the room specified
userOwnsRoom :: UserId -> RoomId -> State -> Bool
userOwnsRoom uid rid =
    anyOf (app . A.rooms . at rid . _Just . roomOwner) (== uid)


disconnectClient :: Concurrent.MVar State -> User -> IO ()
disconnectClient state usr = Concurrent.modifyMVar_ state $ \s -> do
    let uid      = usr ^. userId
        rm       = s ^? userState . at uid . _Just . room . _Just
        newState = s & app . A.users %~ sans uid & userState %~ sans uid

    case rm of
        Just rid | newState & userOwnsRoom uid rid ->
            destroyRoom rid newState usr
        Just rid -> leaveRoom rid newState usr
        _        -> return newState


connectClient :: UserName -> Concurrent.MVar State -> WS.Connection -> IO User
connectClient uname state conn = Concurrent.modifyMVar state $ \s -> do
    let usrs = s ^. app . A.users
        uid  = nextId usrs
        usr  = User {_userName = uname, _userId = uid}
        msg  = encode $ Connected uid
        newState =
            s & app . A.users . at uid ?~ usr & userState . at uid ?~ UserState
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
                    }
                newState =
                    s
                        &  userState
                        .  at uid
                        .  _Just
                        .  room
                        ?~ rid
                        &  app
                        .  A.rooms
                        .  at rid
                        ?~ rm
            broadcastRoom rid newState $ encode $ RoomCreated rm
            return newState
        else return s


joinRoom :: RoomId -> CommandHandler
joinRoom rid s usr = do
    let uid           = usr ^. userId
        userNotInRoom = not (s & userInAnyRoom uid)
        room'         = s ^? app . A.rooms . at rid . _Just
    let newState = if userNotInRoom
            then
                s
                &  app
                .  A.rooms
                .  at rid
                .  _Just
                .  roomUsers
                .  at uid
                ?~ (usr, Nothing)
                &  userState
                .  at uid
                .  _Just
                .  room
                ?~ rid
            else s
    let room'' = newState ^? app . A.rooms . at rid . _Just
    Monad.when userNotInRoom $ case room'' of
        Just r -> do
            let msg = encode $ RoomJoined r
            broadcastRoom rid newState msg
        Nothing -> return ()
    return newState


leaveRoom :: RoomId -> CommandHandler
leaveRoom rid s usr = do
    let uid = usr ^. userId
        inRoom =
            isJust $ s ^? app . A.rooms . at rid . _Just . roomUsers . at uid
    if inRoom
        then do
            let newState =
                    s & app . A.rooms . at rid . _Just . roomUsers %~ sans uid
                newRoom = newState ^? app . A.rooms . at rid . _Just
            case newRoom of
                Just r  -> broadcastRoom rid newState $ encode $ RoomLeft r
                Nothing -> return ()
            return newState
        else return s


destroyRoom :: RoomId -> CommandHandler
destroyRoom rid s usr = do
    let uid    = usr ^. userId
        ownsRm = s & userOwnsRoom uid rid
        msg    = encode $ RoomDestroyed rid
    Monad.when ownsRm $ broadcastRoom rid s msg
    return $ if ownsRm
        then
            s
            &  userState
            .  traverse
            .  room
            .~ Nothing
            &  app
            .  A.rooms
            %~ sans rid
        else s


newStory :: RoomId -> Story -> CommandHandler
newStory rid stry s usr = do
    let uid    = usr ^. userId
        ownsRm = s & userOwnsRoom uid rid
        msg    = encode $ NewStoryCreated rid stry
    Monad.when ownsRm $ broadcastRoom rid s msg
    -- TODO: Check if story already in flight?
    return $ if ownsRm
        then
            s
            &  app
            .  A.rooms
            .  at rid
            .  _Just
            .  roomStory
            .~ stry
            &  app
            .  A.rooms
            .  at rid
            .  _Just
            .  roomUsers
            .  traverse
            .  _2
            .~ Nothing
        else s

makeVote :: RoomId -> Card -> CommandHandler
makeVote rid crd s usr = do
    let uid       = usr ^. userId
        inRoom    = s & userInRoom uid rid
        legalCard = s & anyOf
            (app . A.rooms . at rid . _Just . roomDeck . folded)
            (== crd)
        isLegalVote = inRoom && legalCard
    if not isLegalVote
        then return s
        else do
            let newState =
                    s
                        &  app
                        .  A.rooms
                        .  at rid
                        .  _Just
                        .  roomUsers
                        .  at uid
                        .  _Just
                        .  _2
                        ?~ crd
                votingComplete = newState & allOf
                    (app . A.rooms . at rid . _Just . roomUsers . folded . _2)
                    (\case
                        Just _ -> True
                        _      -> False
                    )
                votes =
                    newState
                        ^.. app
                        .   A.rooms
                        .   at rid
                        .   _Just
                        .   roomUsers
                        .   traverse
                        .   _2
                        .   _Just
                msg = encode $ VotingComplete votes

            Monad.when votingComplete $ broadcastRoom rid s msg
            return $ if votingComplete
                then
                    newState
                    &  app
                    .  A.rooms
                    .  at rid
                    .  _Just
                    .  roomUsers
                    .  traverse
                    .  _2
                    .~ Nothing
                else newState


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
    Vote           rid crd         -> makeVote rid crd s usr
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
        Just (Connect uname) -> return $ Just uname
        _                    -> return Nothing


wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    uname <- getUserName conn
    usr   <- connectClient uname state conn
    Exception.finally (handleRequests state conn usr)
                      (disconnectClient state usr)
