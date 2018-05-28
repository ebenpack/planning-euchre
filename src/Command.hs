{-# LANGUAGE DeriveGeneric #-}
module Command(Command(..), parseCommand) where

import           Card                 (Card)
import           Data.Aeson           (FromJSON, ToJSON (toEncoding), decode,
                                       defaultOptions, genericToEncoding)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import           Deck                 (Deck)
import           GHC.Generics         (Generic)
import           Room                 (Private, RoomId, RoomName)
import           Story                (Story)
import           User                 (User, UserId, UserName)


data Command =
    CreateRoom RoomName Story Deck Private
  | DestroyRoom RoomId
  | RoomDestroyed RoomId
  | JoinRoom RoomId
  | LeaveRoom RoomId
  | RoomJoined RoomId User
  | RoomLeft RoomId User
  | Connect UserName
  | Connected UserId
  | Disconnect
  | Disconnected UserId
  | CreateNewStory RoomId Story
  | NewStoryCreated RoomId Story
  | VotingComplete [Card]
  | Vote RoomId Card deriving (Generic) -- TODO: better name? estimate?

-- TODO: Write explicit ToJSON/FromJSON instances(?)
instance ToJSON Command where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Command

parseCommand :: Text.Text -> Maybe Command
parseCommand = decode . fromStrict . encodeUtf8
