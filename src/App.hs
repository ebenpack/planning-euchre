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

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Map      as Map
import           JSON          (jsonOptions)
import           Room          (Room, RoomId)
import           User          (User, UserId)

type Rooms = Map.Map RoomId Room

type Users = Map.Map UserId User

data App = App { _rooms :: Rooms
               , _users :: Users } deriving (Show)

newApp :: App
newApp = App { _rooms = Map.empty, _users = Map.empty }

makeLenses ''App
deriveJSON jsonOptions ''App
