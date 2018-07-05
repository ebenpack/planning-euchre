{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Backend.HttpApp
  ( app
  ) where

import qualified Control.Concurrent as Concurrent
import           Data.Proxy         (Proxy (Proxy))
import qualified Lucid              as L
import qualified Lucid.Base         as L
import           Miso               (View)
import qualified Miso
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai        as Wai
import           Servant            ((:<|>) (..), (:>))
import qualified Servant

import qualified Common.Model       as Common
import           Common.Room        (RoomId)
import qualified Common.Routes      as Common
import qualified Common.View        as Common

type ServerRoutes = Miso.ToServerRoutes Common.ViewRoutes HtmlPage Common.Action

type ServerAPI = StaticAPI :<|> ServerRoutes :<|> Servant.Raw

type StaticAPI = "static" :> Servant.Raw

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
        L.with
          (L.link_ mempty)
          [ L.makeAttribute "href" "/static/bulma.min.css"
          , L.makeAttribute "rel" "stylesheet"
          , L.makeAttribute "type" "text/css"
          ]
      L.body_ (L.toHtml x)

app :: Wai.Application
app =
  Servant.serve
    (Proxy @ServerAPI)
    (static :<|> serverHandlers :<|> Servant.Tagged page404)
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "static"
    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = signInHandler :<|> joinRoomHandler :<|> roomHandler
    signInHandler :: Servant.Handler (HtmlPage (View Common.Action))
    signInHandler =
      pure $ HtmlPage $ Common.viewModel $ Common.initialModel Common.signInLink
    joinRoomHandler :: Servant.Handler (HtmlPage (View Common.Action))
    joinRoomHandler =
      pure $
      HtmlPage $ Common.viewModel $ Common.initialModel Common.joinRoomLink
    roomHandler :: RoomId -> Servant.Handler (HtmlPage (View Common.Action))
    roomHandler rid =
      pure $
      HtmlPage $ Common.viewModel $ Common.initialModel (Common.roomLink rid)
    page404 :: Wai.Application
    page404 _ respond =
      respond $
      Wai.responseLBS HTTP.status404 [("Content-Type", "text/html")] $
      L.renderBS $ L.toHtml Common.page404View
