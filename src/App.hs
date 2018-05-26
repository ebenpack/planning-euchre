{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module App(
      Rooms
    , Users
    , App
    , newApp
    , rooms
    , users
    , _rooms
    , _users) where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON (parseJSON), ToJSON (toEncoding),
                               genericParseJSON, genericToEncoding)
import qualified Data.Map     as Map
import           GHC.Generics (Generic)
import           JSON         (jsonOptions)
import           Room         (Room, RoomId)
import           User         (User, UserId)

type Rooms = Map.Map RoomId Room

type Users = Map.Map UserId User

data App = App { _rooms :: Rooms
               , _users :: Users } deriving (Generic, Show)


newApp :: App
newApp = App { _rooms = Map.empty, _users = Map.empty }

makeLenses ''App

instance ToJSON App where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON App where
    parseJSON = genericParseJSON jsonOptions
