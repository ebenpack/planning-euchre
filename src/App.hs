{-# LANGUAGE TemplateHaskell #-}
module App(Rooms, Users, App, newApp,_appRooms,_appUsers,getUsers,getUser,addUser,removeUser,addRoom,removeRoom) where

import           Control.Lens (at, makeLensesFor, sans, (%~), (&), (?~), (^.))
import qualified Data.Map     as Map
import           Room         (Room, RoomId, _roomId)
import           User         (User, UserId, _userId)

type Rooms = Map.Map RoomId Room

type Users = Map.Map UserId User

data App = App { rooms  :: Rooms
                , users :: Users } deriving (Show)

newApp :: App
newApp = App { rooms = Map.empty, users = Map.empty }

makeLensesFor [("rooms", "_appRooms"), ("users", "_appUsers")] ''App

-- TODO: Sort out where to put lenses

getUsers :: App -> Users
getUsers s = s ^. _appUsers

getUser :: App -> UserId -> Maybe User
getUser s uid = s ^. (_appUsers . at uid)

addUser :: App -> User -> App
addUser s user = s & (_appUsers . at (user ^. _userId)) ?~ user

removeUser :: App -> UserId -> App
removeUser s uid = s & (_appUsers %~ sans uid)

addRoom :: App -> Room -> App
addRoom s room = s & (_appRooms . at (room ^. _roomId)) ?~ room

removeRoom :: App -> RoomId -> App
removeRoom s rid = s & (_appRooms %~ sans rid)
