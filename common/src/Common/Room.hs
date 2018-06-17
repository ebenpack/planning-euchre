{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Room(
      RoomId
    , RoomName
    , Private
    , Room(
          Room
        , _roomId
        , _roomName
        , _roomOwner
        , _roomUsers
        , _roomStory
        , _roomDeck
        , _roomResult
        , _roomPrivate
    )
    , roomId
    , roomName
    , roomOwner
    , roomUsers
    , roomResult
    , roomStory
    , roomDeck
    , roomPrivate) where

import           Common.Card               (Card)
import           Control.Lens       (makeLenses)
import           Data.Aeson         (FromJSON (parseJSON), ToJSON (toEncoding),
                                     genericParseJSON, genericToEncoding)
import qualified Data.IntMap.Strict as M
import           Common.Deck               (Deck)
import           GHC.Generics       (Generic)
import           Common.JSON               (jsonOptions)
import           Common.Story              (Story)
import           Common.User               (User, UserId)

type Private = Bool

type RoomId = Int

type RoomName = String

type Result = Maybe Card

data Room = Room { _roomId      :: RoomId
                 , _roomName    :: RoomName
                 , _roomOwner   :: UserId
                 , _roomUsers   :: M.IntMap (User, Maybe Card)
                 , _roomResult  :: Result
                 , _roomStory   :: Story
                 , _roomDeck    :: Deck
                 , _roomPrivate :: Private } deriving (Show, Generic)

makeLenses ''Room

instance ToJSON Room where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON Room where
    parseJSON = genericParseJSON jsonOptions
