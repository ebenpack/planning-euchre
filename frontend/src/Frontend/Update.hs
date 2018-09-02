{-# LANGUAGE BangPatterns #-}

module Frontend.Update where

import           Control.Lens               (contains, filtered, folded, noneOf,
                                             toListOf, (%=), (&), (+=), (-=),
                                             (.=), (<>=), (?=), (^.), (^..),
                                             _Just)
import           Control.Monad.Writer.Class (tell)
import           Data.Char                  (isDigit)
import           Data.Maybe                 (fromMaybe)
import           Miso                       (Transition, WebSocket (..),
                                             pushURI, scheduleIO, send, (<#))
import           Miso.String                (MisoString, fromMisoString)
import           Text.Read                  (readMaybe)

import           Debug.Trace                (traceShow)

import           Common.Card                (Card (..))
import qualified Common.Command             as Command
import           Common.Deck                (fibonacci)
import qualified Common.Model               as Model
import           Common.Room                as Room
import qualified Common.Routes              as Routes
import           Common.User                (UserName)

updateModel :: Model.Action -> Transition Model.Action Model.Model ()
updateModel Model.NoOp            = pure ()
updateModel (Model.ChangeURI uri) = scheduleIO $ do
  pushURI uri
  pure Model.NoOp
updateModel (Model.HandleURI uri  ) = Model.uri .= uri
updateModel (Model.Connect   uname) = scheduleIO $ do
  send $ Command.Connect uname
  pure Model.NoOp
updateModel (Model.Connected uid) = do
    Model.userId ?= uid
    scheduleIO $ pure $ Model.ChangeURI Routes.joinRoomLink
updateModel (Model.SendMessage msg) = scheduleIO $ do
  send msg
  pure Model.NoOp
updateModel (Model.JoinRoom rid) = do
  let msg = Command.JoinRoom rid
  scheduleIO $ do
    send msg
    pure Model.NoOp
updateModel (Model.RoomJoined rm) = do
  let rid = rm ^. Room.roomId
  Model.room ?= rm
  scheduleIO $ pure $ Model.ChangeURI $ Routes.roomLink rid
updateModel (Model.CreateRoom rname stry deck private) = do
  let msg = Command.CreateRoom rname stry deck private
  scheduleIO $ do
    send msg
    pure Model.NoOp
updateModel (Model.Vote crd) = do
    let msg = Command.Vote crd
    Model.vote ?= crd
    scheduleIO $ do
      send msg
      pure Model.NoOp
updateModel (Model.VotingReadyToClose _) =
    Model.room . _Just . Room.roomState .= Room.VotingComplete
updateModel (Model.VotingClosed rm) = Model.room ?= rm
-- Sign in
updateModel (Model.SignInUpdateUserName    n        ) = Model.userName .= n
-- Create room
updateModel (Model.CreateRoomUpdateName    n        ) = Model.roomName .= n
updateModel (Model.CreateRoomUpdateStory   s        ) = Model.roomStory .= s
updateModel (Model.CreateRoomUpdatePrivacy p        ) = Model.roomPrivate .= p
updateModel (Model.CreateRoomUpdateDeck card checked) = if checked
  then Model.roomDeck <>= [card]
  else Model.roomDeck %= (filter (/= card))
-- Join room
updateModel (Model.JoinRoomUpdateId rid) = Model.roomId .= rid
updateModel (Model.NewStoryCreated rm) = do
    Model.room ?= rm
    Model.vote .= Nothing
updateModel (Model.CreateNewStory rid stry) = do
    let msg = Command.CreateNewStory rid $ fromMisoString stry
    scheduleIO $ do
      send msg
      pure Model.NoOp
updateModel (Model.RoomDestroyed) = do
    scheduleIO $ pure $ Model.ChangeURI Routes.joinRoomLink
updateModel (Model.CloseVote rid) = do
    scheduleIO $ do
        send $ Command.CloseVote rid
        pure Model.NoOp


socketHandler :: WebSocket Command.Command -> Model.Action
socketHandler WebSocketOpen = Model.NoOp
socketHandler (WebSocketClose _ _ _                        ) = Model.NoOp
socketHandler (WebSocketError   _                          ) = Model.NoOp
socketHandler (WebSocketMessage (Command.RoomCreated   rm )) = Model.RoomJoined rm
socketHandler (WebSocketMessage (Command.RoomDestroyed rid)) = Model.RoomDestroyed
socketHandler (WebSocketMessage (Command.RoomJoined    rm )) = Model.RoomJoined rm
socketHandler (WebSocketMessage (Command.RoomLeft rm )) = Model.RoomJoined rm
socketHandler (WebSocketMessage (Command.Connected uid)) = Model.Connected uid
socketHandler (WebSocketMessage (Command.Disconnected uid)) = Model.NoOp
socketHandler (WebSocketMessage (Command.NewStoryCreated rm)) = Model.NewStoryCreated rm
socketHandler (WebSocketMessage (Command.VotingReadyToClose rid)) = Model.VotingReadyToClose rid
socketHandler (WebSocketMessage (Command.VotingClosed rm)) = Model.VotingClosed rm
