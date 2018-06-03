module App.Effects where

import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Exception (EXCEPTION)
import WebSocket (WEBSOCKET)

type AppEffects fx = (ws :: WEBSOCKET, err :: EXCEPTION, ajax :: AJAX, dom :: DOM, history :: HISTORY | fx)
