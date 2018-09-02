{-# LANGUAGE TemplateHaskell     #-}

module Backend.State
    ( UserState(UserState, _connection, _room)
    , State(State, _app, _userState)
    , CommandHandler
    , connection
    , room
    , app
    , userState
    , appRoomStory
    , appRoomVotes
    , appRoomUsers
    , appRoomState
    , appRoom
    , appUser
    , appRoomOwner
    , userStateConnection
    , userStateRoom
    , userStateUser
    , appRoomUser
    , appRoomVote
    , appRoomDeck
    , userInAnyRoom
    , userInRoom
    , userOwnsRoom
    )
where

import qualified Network.WebSockets            as WS
import qualified Data.IntMap.Strict            as M
import           Control.Lens                   ( at
                                                , makeLenses
                                                , _Just
                                                , _2
                                                , _1
                                                , anyOf
                                                , folded
                                                , (^.)
                                                )

import qualified Common.App                    as App
                                                ( App
                                                , rooms
                                                , users
                                                )
import           Common.Card                    ( Card )
import           Common.Deck                    ( Deck )
import           Common.Room                   as Room
                                                ( RoomId
                                                , Room
                                                , RoomState
                                                , roomDeck
                                                , roomOwner
                                                , roomStory
                                                , roomState
                                                , roomUsers
                                                )
import           Common.Story                   ( Story )
import           Common.User                    ( User
                                                , UserId
                                                , userId
                                                )


data UserState = UserState { _connection :: WS.Connection
                           , _room       :: Maybe RoomId}

data State = State { _app :: App.App, _userState :: M.IntMap UserState }

type CommandHandler = State -> User -> IO State

makeLenses ''UserState
makeLenses ''State

-------------
-- UserState
-------------
userStateConnection
    :: Applicative f
    => Int
    -> (WS.Connection -> f WS.Connection)
    -> State
    -> f State
userStateConnection uid = userState . at uid . _Just . connection


userStateRoom
    :: Applicative f
    => Int
    -> (Maybe RoomId -> f (Maybe RoomId))
    -> State
    -> f State
userStateRoom uid = userState . at uid . _Just . room


userStateUser
    :: Functor f
    => Int
    -> (Maybe UserState -> f (Maybe UserState))
    -> State
    -> f State
userStateUser uid = userState . at uid

-------------
-- App Users
-------------
appUser
    :: Functor f => Int -> (Maybe User -> f (Maybe User)) -> State -> f State
appUser uid = app . App.users . at uid


-------------
-- App Room
-------------
appRoom
    :: Functor f => Int -> (Maybe Room -> f (Maybe Room)) -> State -> f State
appRoom rid = app . App.rooms . at rid


appRoomStory :: Applicative f => Int -> (Story -> f Story) -> State -> f State
appRoomStory rid = appRoom rid . _Just . roomStory


appRoomUsers
    :: Applicative f
    => Int
    -> (M.IntMap (User, Maybe Card) -> f (M.IntMap (User, Maybe Card)))
    -> State
    -> f State
appRoomUsers rid = appRoom rid . _Just . Room.roomUsers


appRoomUser
    :: Applicative f
    => Int
    -> Int
    -> (Maybe (User, Maybe Card) -> f (Maybe (User, Maybe Card)))
    -> State
    -> f State
appRoomUser rid uid = appRoomUsers rid . at uid


appRoomVotes
    :: Applicative f
    => Int
    -> (Maybe Card -> f (Maybe Card))
    -> State
    -> f State
appRoomVotes rid = appRoomUsers rid . traverse . _2


appRoomVote
    :: Applicative f
    => Int
    -> Int
    -> (Maybe Card -> f (Maybe Card))
    -> State
    -> f State
appRoomVote rid uid = appRoomUser rid uid . _Just . _2


appRoomState
    :: Applicative f => Int -> (RoomState -> f RoomState) -> State -> f State
appRoomState rid = appRoom rid . _Just . roomState


appRoomDeck :: Applicative f => Int -> (Deck -> f Deck) -> State -> f State
appRoomDeck rid = appRoom rid . _Just . roomDeck


appRoomOwner :: Applicative f => Int -> (UserId -> f UserId) -> State -> f State
appRoomOwner rid = appRoom rid . _Just . Room.roomOwner


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
userInRoom uid rid = anyOf (appRoomUsers rid . folded . _1 . userId) (== uid)


-- |Predicate function indicating whether a given user owns the room specified
userOwnsRoom :: UserId -> RoomId -> State -> Bool
userOwnsRoom uid rid = anyOf (appRoomOwner rid) (== uid)
