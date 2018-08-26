module Main where

import Data.Aeson (FromJSON, decode)
import qualified Frontend.Update as Frontend
import Miso
  ( App(..)
  , Protocols(..)
  , Transition
  , URL(..)
  , defaultEvents
  , fromTransition
  , miso
  , uriSub
  , websocketSub
  )

import qualified Common.Command as Command
import qualified Common.Model as Model
import qualified Common.View as View

main :: IO ()
main = do
  miso $ \currentURI -> App
    { initialAction = Model.NoOp
    , model         = Model.initialModel currentURI
    , update        = fromTransition . Frontend.updateModel
    , view          = View.viewModel
    , events        = defaultEvents
    , subs          = [ uriSub Model.HandleURI
                      , websocketSub uri protocols Frontend.socketHandler
                      ]
    , mountPoint    = Nothing
    }
 where
  uri       = URL "ws://localhost:3000"
  protocols = Protocols []
