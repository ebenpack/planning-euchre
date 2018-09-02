{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Room
    ( RoomId
    , RoomName
    , Private
    , Room
        ( Room
        , _roomId
        , _roomName
        , _roomOwner
        , _roomUsers
        , _roomStory
        , _roomDeck
        , _roomResult
        , _roomPrivate
        , _roomState
        )
    , RoomState (Voting, Results)
    , roomId
    , roomName
    , roomOwner
    , roomUsers
    , roomResult
    , roomStory
    , roomDeck
    , roomPrivate
    , roomState
    )
where

import           Common.Card        (Card)
import           Common.Deck        (Deck)
import           Common.JSON        (jsonOptions)
import           Common.Story       (Story)
import           Common.User        (User, UserId)
import           Control.Lens       (makeLenses)
import           Data.Aeson         (FromJSON (parseJSON), ToJSON (toEncoding),
                                     defaultOptions, genericParseJSON,
                                     genericToEncoding)
import qualified Data.IntMap.Strict as M
import           GHC.Generics       (Generic)

type Private = Bool

type RoomId = Int

type RoomName = String

type Result = Maybe Card

data RoomState = Voting | Results deriving (Show, Generic, Eq)

instance ToJSON RoomState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RoomState

data Room = Room { _roomId      :: RoomId
                 , _roomName    :: RoomName
                 , _roomOwner   :: UserId
                 , _roomUsers   :: M.IntMap (User, Maybe Card)
                 , _roomResult  :: Result
                 , _roomStory   :: Story
                 , _roomDeck    :: Deck
                 , _roomPrivate :: Private
                 , _roomState   :: RoomState } deriving (Show, Generic, Eq)

makeLenses ''Room

instance ToJSON Room where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON Room where
    parseJSON = genericParseJSON jsonOptions
