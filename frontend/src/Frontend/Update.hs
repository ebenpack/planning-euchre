{-# LANGUAGE BangPatterns #-}

module Frontend.Update where

import           Control.Lens               (contains, filtered, folded, noneOf,
                                             toListOf, (%=), (&), (+=), (-=),
                                             (.=), (<>=), (?=), (^.), (^..))
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
updateModel Model.NoOp = pure ()
updateModel (Model.ChangeURI uri) =
  scheduleIO $ do
    pushURI uri
    pure Model.NoOp
updateModel (Model.HandleURI uri) = Model.uri .= uri
updateModel (Model.Connect uname) =
  scheduleIO $ do
    send $ Command.Connect uname
    pure Model.NoOp
updateModel (Model.SendMessage msg) =
  scheduleIO $ do
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
-- Sign in
updateModel (Model.SignInUpdateUserName n) = Model.userName .= n
-- Create room
updateModel (Model.CreateRoomUpdateName n) = Model.roomName .= n
updateModel (Model.CreateRoomUpdateStory s) = Model.roomStory .= s
updateModel (Model.CreateRoomUpdatePrivacy p) = Model.roomPrivate .= p
updateModel (Model.CreateRoomUpdateDeck card checked) =
  if checked
    then Model.roomDeck <>= [card]
    else Model.roomDeck %= (filter (/= card))
-- Join room
updateModel (Model.JoinRoomUpdateId rid) = Model.roomId .= rid

socketHandler :: WebSocket Command.Command -> Model.Action
socketHandler WebSocketOpen = Model.NoOp
socketHandler (WebSocketClose _ _ _) = Model.NoOp
socketHandler (WebSocketError _) = Model.NoOp
socketHandler (WebSocketMessage (Command.RoomCreated rm)) = Model.RoomJoined rm
socketHandler (WebSocketMessage (Command.RoomDestroyed rid)) = Model.NoOp
socketHandler (WebSocketMessage (Command.RoomJoined rm)) = Model.RoomJoined rm
socketHandler (WebSocketMessage (Command.RoomLeft rid usr)) = Model.NoOp
socketHandler (WebSocketMessage (Command.Connected uid)) =
  Model.ChangeURI Routes.joinRoomLink
socketHandler (WebSocketMessage (Command.Disconnected uid)) = Model.NoOp
socketHandler (WebSocketMessage (Command.NewStoryCreated rid stry)) = Model.NoOp
socketHandler (WebSocketMessage (Command.VotingComplete crds)) = Model.NoOp
