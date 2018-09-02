module Main where

import           Data.Aeson                       (FromJSON, decode)
import qualified Frontend.Update                  as Frontend
import           Language.Javascript.JSaddle.Warp as JSaddle
import           Miso                             (App (..), Protocols (..),
                                                   Transition, URL (..),
                                                   defaultEvents,
                                                   fromTransition,
                                                   getCurrentURI, miso,
                                                   startApp, uriSub,
                                                   websocketSub)

import qualified Common.Command                   as Command
import qualified Common.Model                     as Model
import qualified Common.View                      as View

main :: IO ()
main = JSaddle.run 8080 $ do
    currentURI <- getCurrentURI
    miso (\_ -> App
      { initialAction = Model.NoOp
      , model         = Model.initialModel currentURI
      , update        = fromTransition . Frontend.updateModel
      , view          = View.viewModel
      , events        = defaultEvents
      , subs          = [ uriSub Model.HandleURI
                        , websocketSub uri protocols Frontend.socketHandler
                        ]
      , mountPoint    = Nothing
      })
 where
  uri       = URL "ws://localhost:3000"
  protocols = Protocols []
