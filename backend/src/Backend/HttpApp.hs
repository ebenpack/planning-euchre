{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Backend.HttpApp (app) where

import           Data.Proxy
import qualified Lucid              as L
import qualified Lucid.Base         as L
import           Miso               (View)
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai        as Wai
import           Servant            ((:<|>) (..), (:>))
import qualified Servant

import qualified Common.Model       as Common
import qualified Common.Routes      as Common
import qualified Common.View        as Common


app :: Wai.Application
app =
  Servant.serve
    (Proxy :: Proxy ServerAPI)
    (static :<|> serverHandlers :<|> Servant.Tagged page404)
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "static"
    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = homeServer :<|> roomServer
    homeServer :: Servant.Handler (HtmlPage (View Common.Action))
    homeServer =
      pure $ HtmlPage $ Common.viewModel $ Common.initialModel Common.homeLink
    roomServer :: Servant.Handler (HtmlPage (View Common.Action))
    roomServer =
      pure $
      HtmlPage $ Common.viewModel $ Common.initialModel Common.roomLink
    page404 :: Wai.Application
    page404 _ respond =
      respond $
      Wai.responseLBS HTTP.status404 [("Content-Type", "text/html")] $
      L.renderBS $ L.toHtml Common.page404View


newtype HtmlPage a =
  HtmlPage a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (HtmlPage x) =
    L.doctypehtml_ $ do
      L.head_ $ do
        L.title_ "Planning Euchre"
        L.meta_ [L.charset_ "utf-8"]
        L.with
          (L.script_ mempty)
          [ L.makeAttribute "src" "/static/all.js"
          , L.makeAttribute "async" mempty
          , L.makeAttribute "defer" mempty
          ]
      L.body_ (L.toHtml x)


type ServerRoutes = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

type ServerAPI
   = StaticAPI :<|> (ServerRoutes :<|> Servant.Raw)

type StaticAPI = "static" :> Servant.Raw
