module Main where

import qualified Control.Concurrent            as Concurrent
import           Data.IntMap.Strict             ( empty )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Handler.WebSockets
                                               as WS
import qualified Network.Wai.Middleware.Gzip   as Wai
import qualified Network.Wai.Middleware.RequestLogger
                                               as Wai
import qualified Network.WebSockets            as WS
import qualified System.IO                     as IO

import           Backend.HttpApp                ( app )
import           Backend.WsApp                  ( State(State)
                                                , wsApp
                                                , _app
                                                , _userState
                                                )
import           Common.App                     ( newApp )

main :: IO ()
main = do
    state <- Concurrent.newMVar $ State {_app = newApp, _userState = empty}
    IO.hPutStrLn IO.stderr "Running on port http://127.0.0.1:3000 ..."
    Warp.run 3000 $ Wai.logStdout $ compress $ WS.websocketsOr
        WS.defaultConnectionOptions
        (wsApp state)
        app
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def { Wai.gzipFiles = Wai.GzipCompress }
