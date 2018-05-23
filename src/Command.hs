{-# LANGUAGE TemplateHaskell #-}
module Command(Command(..), parseCommand) where

import           Data.Aeson           (decode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import           Room                 (RoomId)
import           User                 (UserId, UserName)


data Command =
    CreateRoom
  | JoinRoom RoomId
  | Connect UserName
  | Connected UserId
  | Disconnect
  | NewStory

-- TODO: Write explicit ToJSON/FromJSON instances(?)
deriveJSON defaultOptions ''Command

parseCommand :: Text.Text -> Maybe Command
parseCommand = decode . fromStrict . encodeUtf8
