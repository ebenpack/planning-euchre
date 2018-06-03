module Main where

import           App                            (newApp)
import qualified Control.Concurrent             as Concurrent
import           Data.IntMap.Strict             (empty)
import           HttpApp                        (httpApp)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           Web.Scotty                     (scottyApp)
import           WsApp                          (State (State), wsApp, _app,
                                                 _userState)

main :: IO ()
main = do
    sapp <- scottyApp httpApp
    state <- Concurrent.newMVar $ State { _app=newApp, _userState=empty }
    Warp.run 3000 $ WS.websocketsOr
      WS.defaultConnectionOptions
        (wsApp state)
        sapp
