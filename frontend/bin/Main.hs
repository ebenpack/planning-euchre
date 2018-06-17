module Main where

import           Data.Aeson      (FromJSON, decode)
import qualified Frontend.Update as Frontend
import           Miso            (App (..), defaultEvents, fromTransition,
                                  uriSub)

import qualified Common.Model    as Common
import qualified Common.View     as Common

main :: IO ()
main = do
  Miso.miso $ \currentURI ->
    App
    { initialAction = Common.NoOp
    , model = Common.initialModel currentURI
    , update = fromTransition . Frontend.updateModel
    , view = Common.viewModel
    , events = defaultEvents
    , subs = [ uriSub Common.HandleURI ]
    , mountPoint = Nothing
    }
