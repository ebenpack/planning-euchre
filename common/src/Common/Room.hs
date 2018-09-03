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
        , _roomPrivate
        , _roomState
        )
    , RoomState (VotingOpen, VotingComplete, VotingClosed)
    , roomId
    , roomName
    , roomOwner
    , roomUsers
    , roomStory
    , roomDeck
    , roomPrivate
    , roomState
    )
where

import           Control.Lens       (makeLenses)
import           Data.Aeson         (FromJSON (parseJSON), ToJSON (toEncoding),
                                     defaultOptions, genericParseJSON,
                                     genericToEncoding)
import qualified Data.IntMap.Strict as M
import qualified Data.Text          as Text
import           GHC.Generics       (Generic)

import           Common.Card        (Card)
import           Common.Deck        (Deck)
import           Common.JSON        (jsonOptions)
import           Common.Story       (Story)
import           Common.User        (User, UserId)


type Private = Bool

type RoomId = Int

type RoomName = Text.Text

type RoomUsers = M.IntMap (User, Maybe Card)

data RoomState = VotingOpen | VotingComplete | VotingClosed deriving (Show, Generic, Eq)

instance ToJSON RoomState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RoomState

data Room = Room { _roomId      :: RoomId
                 , _roomName    :: RoomName
                 , _roomOwner   :: UserId
                 , _roomUsers   :: RoomUsers
                 , _roomStory   :: Story
                 , _roomDeck    :: Deck
                 , _roomPrivate :: Private
                 , _roomState   :: RoomState } deriving (Show, Generic, Eq)

makeLenses ''Room

instance ToJSON Room where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON Room where
    parseJSON = genericParseJSON jsonOptions
