module Main where

import qualified Control.Concurrent             as Concurrent
import           HttpApp                        (httpApp)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           Types
import           Web.Scotty                     (scottyApp)
import           WsApp                          (wsApp)

main :: IO ()
main = do
    sapp <- scottyApp httpApp
    state <- Concurrent.newMVar newApp
    Warp.run 3000 $ WS.websocketsOr
      WS.defaultConnectionOptions
        (wsApp state)
        sapp
