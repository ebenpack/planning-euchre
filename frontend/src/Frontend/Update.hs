module Frontend.Update where

import           Control.Lens   (use, (+=), (-=), (.=))
import           Miso           (Protocols (..), Transition, URL (..),
                                 WebSocket (..), pushURI, scheduleIO,
                                 scheduleSub, send, toTransition, websocketSub,
                                 (<#))
import           Miso.String    (fromMisoString)

import qualified Common.Command as Command
import qualified Common.Model   as Common

updateModel :: Common.Action -> Transition Common.Action Common.Model ()
updateModel Common.NoOp = pure ()
updateModel (Common.ChangeURI uri) = scheduleIO $ do
        pushURI uri
        pure Common.NoOp
updateModel (Common.HandleURI uri) = Common.uri .= uri
updateModel (Common.SendMessage msg) = scheduleIO $ do
    send msg
    pure Common.NoOp
updateModel (Common.Connect usrnm) = scheduleSub $ do
        let uri = URL "ws://localhost:3000"
            protocols = Protocols [ ]
        websocketSub uri protocols socketHandler
updateModel Common.Connected = do
    username <- use Common.userName
    let msg = Command.Connect username
    scheduleIO $ do
        send msg
        pure Common.NoOp
updateModel (Common.UpdateName name) = Common.userName .= fromMisoString name


socketHandler :: WebSocket Command.Command -> Common.Action
socketHandler WebSocketOpen = Common.Connected
socketHandler (WebSocketClose _ _ _) = Common.NoOp
socketHandler (WebSocketError _) = Common.NoOp
socketHandler (WebSocketMessage (Command.RoomDestroyed rid)) = Common.NoOp
socketHandler (WebSocketMessage (Command.RoomJoined rid usr)) = Common.NoOp
socketHandler (WebSocketMessage (Command.RoomLeft rid usr)) = Common.NoOp
socketHandler (WebSocketMessage (Command.Connected uid)) = Common.NoOp
socketHandler (WebSocketMessage (Command.Disconnected uid)) = Common.NoOp
socketHandler (WebSocketMessage (Command.NewStoryCreated rid stry)) = Common.NoOp
socketHandler (WebSocketMessage (Command.VotingComplete crds)) = Common.NoOp
