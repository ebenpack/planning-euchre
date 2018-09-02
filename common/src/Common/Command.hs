{-# LANGUAGE DeriveGeneric #-}
module Common.Command
    ( Command(..)
    , parseCommand
    )
where

import           Common.Card          (Card)
import           Common.Deck          (Deck)
import           Common.Room          (Private, Room, RoomId, RoomName)
import           Common.Story         (Story)
import           Common.User          (UserId, UserName)
import           Data.Aeson           (FromJSON, ToJSON (toEncoding), decode,
                                       defaultOptions, genericToEncoding)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import           GHC.Generics         (Generic)

data Command =
    CreateRoom RoomName Story Deck Private
  | RoomCreated Room
  | DestroyRoom RoomId
  | RoomDestroyed RoomId
  | JoinRoom RoomId
  | LeaveRoom RoomId
  | RoomJoined Room
  | RoomLeft Room
  | Connect UserName
  | Connected UserId
  | Disconnect
  | Disconnected UserId
  | CreateNewStory RoomId Story
  | NewStoryCreated Room
  | VotingComplete Room
  | Vote Card deriving (Generic, Show, Eq)

-- TODO: Write explicit ToJSON/FromJSON instances(?)
instance ToJSON Command where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Command

parseCommand :: Text.Text -> Maybe Command
parseCommand = decode . fromStrict . encodeUtf8
