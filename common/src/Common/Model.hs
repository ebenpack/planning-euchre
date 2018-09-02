{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common.Model where

import           Control.Lens
import           Miso.String    (MisoString)
import qualified Servant.API    as Servant

import           Common.Card    (Card)
import           Common.Command (Command (..))
import           Common.Deck    (Deck)
import           Common.Room    (Private, Room, RoomId, RoomName)
import           Common.Story   (Story)
import           Common.User    (UserId, UserName)

data Model = Model
  { _uri         :: !Servant.URI
  , _userName    :: !MisoString
  , _roomName    :: !MisoString
  , _roomId      :: !MisoString
  , _roomDeck    :: !Deck
  , _roomStory   :: !MisoString
  , _userId      :: !(Maybe UserId)
  , _roomPrivate :: !Bool
  , _vote        :: !(Maybe Card)
  , _room        :: !(Maybe Room)
  } deriving (Eq, Show)

makeLenses ''Model

initialModel :: Servant.URI -> Model
initialModel initialUri = Model
    { _uri         = initialUri
    , _userName    = ""
    , _roomName    = ""
    , _roomDeck    = []
    , _roomId      = ""
    , _roomStory   = ""
    , _userId      = Nothing
    , _roomPrivate = False
    , _vote        = Nothing
    , _room        = Nothing
    }

data Action
  = NoOp
  | ChangeURI !Servant.URI
  | HandleURI !Servant.URI
  | HandleWebSocket Command
  | SendMessage Command
  | CreateRoom RoomName Story Deck Private
  | RoomDestroyed
  | JoinRoom RoomId
  | RoomJoined Room
  | Connect UserName
  | Connected UserId
  | Vote Card
  | UpdateMessage MisoString
  | SignInUpdateUserName MisoString
  | CreateRoomUpdateStory MisoString
  | CreateRoomUpdateName MisoString
  | CreateRoomUpdatePrivacy Bool
  | CreateRoomUpdateDeck Card Bool
  | JoinRoomUpdateId MisoString
  | VotingReadyToClose RoomId
  | CloseVote RoomId
  | VotingClosed Room
  | CreateNewStory RoomId MisoString
  | NewStoryCreated Room
  deriving (Show, Eq)
