{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common.Model where

import           Control.Lens
import           Miso.String    (MisoString)
import qualified Servant.API    as Servant

import           Common.Command (Command (..))
import           Common.User    (UserName)


data Model = Model
  { _uri      :: !Servant.URI
  , _userName :: !UserName
  } deriving (Eq, Show)

makeLenses ''Model

initialModel :: Servant.URI -> Model
initialModel initialUri = Model {_uri = initialUri, _userName = ""}

data Action
  = NoOp
  | ChangeURI !Servant.URI
  | HandleURI !Servant.URI
  | HandleWebSocket Command
  | SendMessage Command
  | Connect UserName
  | Connected
  | UpdateMessage MisoString
  | UpdateName MisoString
  deriving (Show, Eq)
