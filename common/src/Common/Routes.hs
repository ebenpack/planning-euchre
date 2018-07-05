{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Common.Routes where

import           Data.Proxy          (Proxy (..))
import           Miso                (View)
import           Servant.API         ((:<|>) (..), (:>), Capture)
import qualified Servant.API         as Servant
import qualified Servant.Utils.Links as Servant

import           Common.Model
import           Common.Room         (RoomId)

type ViewRoutes = SignIn :<|> JoinRoom :<|> Room

type SignIn = View Action

type JoinRoom = "room" :> View Action
 -- Capture "roomid" RoomId :>

type Room = "room" :> Capture "roomid" RoomId :> View Action

signInLink :: Servant.URI
signInLink =
  Servant.linkURI $
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @SignIn)

joinRoomLink :: Servant.URI
joinRoomLink =
  Servant.linkURI $
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @JoinRoom)

roomLink :: RoomId -> Servant.URI
roomLink rid =
  Servant.linkURI $
  Servant.safeLink (Proxy @ViewRoutes) (Proxy @Room) rid
