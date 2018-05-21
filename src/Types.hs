{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Types where

import           Control.Lens         (at, makeLensesFor, sans, (%~), (&), (?~),
                                       (^.))
import           Data.Aeson           (decode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Map             as Map
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)

---------------
-- User
---------------

type UserName = Text.Text

type UserId = Int

data User = User { userId   :: UserId
                 , userName :: UserName } deriving (Show)


---------------
-- Card
---------------

data Card =
      Coffee
    | Half
    | One
    | Two
    | Three
    | Five
    | Eight
    | Thirteen
    | Twenty
    | Forty
    | OneHundred
    | Unknown
    | Infinity

instance Show Card where
    show Coffee     = "☕"
    show Half       = "½"
    show One        = "1"
    show Two        = "2"
    show Three      = "3"
    show Five       = "5"
    show Eight      = "8"
    show Thirteen   = "13"
    show Twenty     = "20"
    show Forty      = "40"
    show OneHundred = "100"
    show Unknown    = "?"
    show Infinity   = "∞"

---------------
-- Deck
---------------

newtype Deck = Deck [Card] deriving (Show)

---------------
-- Story
---------------
type Story = String

---------------
-- Room
---------------

type RoomId = Int

type RoomName = String

data Room = Room { roomId  :: RoomId
                 , name    :: RoomName
                 , owner   :: UserId
                 , users   :: Map.Map UserId User
                 , story   :: Story
                 , deck    :: Deck
                 , private :: Bool } deriving (Show)



---------------
-- App
---------------

type Rooms = Map.Map RoomId Room

type Users = Map.Map UserId User

data App = App { rooms :: Rooms
               , users :: Users } deriving (Show)

newApp :: App
newApp = App { rooms = Map.empty, users = Map.empty }

---------------
-- Commands
---------------

data Command =
    CreateRoom
  | JoinRoom RoomId
  | Connect UserName
  | Connected UserId
  | Disconnect
  | NewStory

---------------
-- TH Biz
---------------

concat <$> mapM (deriveJSON defaultOptions) [''User, ''Card, ''Deck, ''Room, ''App, ''Command]
concat <$> mapM (uncurry makeLensesFor) [
      ([("userName", "_userName"), ("userId", "_userId")], ''User)
    , ([("name", "_roomName"), ("users", "_roomUsers"), ("story", "_roomStory"), ("deck", "_roomDeck"), ("private", "_roomPrivate"), ("roomId", "_roomId")], ''Room)
    , ([("rooms", "_appRooms"), ("users", "_appUsers")], ''App)]

---------------
-- Lenses
---------------

getUsers :: App -> Users
getUsers s = s ^. _appUsers

getUser :: App -> UserId -> Maybe User
getUser a uid = a ^. (_appUsers . at uid)

addUser :: App -> User -> App
addUser s user = s & (_appUsers . at (user ^. _userId)) ?~ user

removeUser :: App -> UserId -> App
removeUser s uid = s & (_appUsers %~ sans uid)

addRoom :: App -> Room -> App
addRoom s room = s & (_appRooms . at (room ^. _roomId)) ?~ room

removeRoom :: App -> RoomId -> App
removeRoom s rid = s & (_appRooms %~ sans rid)

---------------
-- Parsers
---------------

parseCommand :: Text.Text -> Maybe Command
parseCommand = decode . fromStrict . encodeUtf8
