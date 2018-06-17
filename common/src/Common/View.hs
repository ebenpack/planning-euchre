module Common.View where

import           Data.Proxy     (Proxy (..))
import           Data.Text      (pack, unpack)
import           Miso           (View)
import qualified Miso
import           Miso.Html
import qualified Miso.String    as Miso
import           Servant.API    ((:<|>) (..))

import qualified Common.Command as Command
import           Common.Model
import           Common.Routes


viewModel :: Model -> View Action
viewModel model = view
  where
    view =
      either (const page404View) id $
      Miso.runRoute (Proxy :: Proxy ViewRoutes) handlers _uri model
    handlers = homeView :<|> roomView


homeView :: Model -> View Action
homeView m =
  div_
    []
    [ div_
        []
        [ input_ [ onInput UpdateName
                 , value_ $ Miso.ms $ unpack $ _userName m]
        , button_ [onClick $ Connect $ _userName m] [text "Connect"]
        ]
    , button_ [onClick $ ChangeURI roomLink] [text "Go to /room"]
    ]


roomView :: Model -> View Action
roomView m =
    div_
    []
    [ div_
        []
        [ input_ [ onInput UpdateName
                 , value_ $ Miso.ms $ unpack $ _userName m]
        , button_ [onClick $ Connect $ _userName m] [text "Connect"]
        ]
    , button_ [onClick $ ChangeURI homeLink] [text "Go to /"]
    ]

-- Handle 404 errors.
page404View :: View Action
page404View = text "Whoopsy daisy... It looks like that page cannot be found. Sorry about that."
