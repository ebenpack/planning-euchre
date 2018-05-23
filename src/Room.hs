{-# LANGUAGE TemplateHaskell #-}
module Room(
      RoomId
    , RoomName
    , Room
    , _name
    , _users
    , _story
    , _deck
    , _private
    , _roomId
    , roomId
    , name
    , owner
    , users
    , story
    , deck
    , private) where

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Map      as Map
import           Deck          (Deck)
import           JSON          (jsonOptions)
import           Story         (Story)
import           User          (User, UserId)


type RoomId = Int

type RoomName = String

data Room = Room { _roomId  :: RoomId
                 , _name    :: RoomName
                 , _owner   :: UserId
                 , _users   :: Map.Map UserId User
                 , _story   :: Story
                 , _deck    :: Deck
                 , _private :: Bool } deriving (Show)

makeLenses ''Room

deriveJSON jsonOptions ''Room
