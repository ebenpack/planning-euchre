{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Common.Routes where

import           Data.Proxy          (Proxy (..))
import           Miso                (View)
import           Servant.API         ((:<|>) (..), (:>))
import qualified Servant.API         as Servant
import qualified Servant.Utils.Links as Servant

import           Common.Model

type ViewRoutes = Home :<|> Room

type Home = View Action

type Room = "room" :> View Action

homeLink :: Servant.URI
homeLink =
  Servant.linkURI $
  Servant.safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Home)


roomLink :: Servant.URI
roomLink =
  Servant.linkURI $
  Servant.safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Room)
