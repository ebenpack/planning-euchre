{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Common.View where

import           Data.Proxy    (Proxy (..))
import           Miso          (View)
import qualified Miso
import           Miso.Html
import qualified Miso.String   as Miso
import           Servant.API   ((:<|>) (..))
import           Text.Read     (readMaybe)

import           Common.Card   (Card (..))
import           Common.Deck   (Deck)
import           Common.Model  as Model
import           Common.Room   as Room
import           Common.Routes as Routes

viewModel :: Model.Model -> View Model.Action
viewModel model = view
  where
    view =
      either (const page404View) id $
      Miso.runRoute (Proxy @ViewRoutes) handlers _uri model
    handlers ::
         (Model -> View Action) :<|> (Model -> View Action) :<|> (Room.RoomId -> Model -> View Action)
    handlers = signInView :<|> joinRoomView :<|> roomView

signInView :: Model.Model -> View Model.Action
signInView m =
  div_
    []
    [ div_
        []
        [ div_
            []
            [ h2_ [] [text "Sign in"]
                -- User name
            , label_ [for_ signInUserNameId, class_ "label"] [text "User Name:"]
            , input_
                [ id_ signInUserNameId
                , class_ "input"
                , onInput Model.SignInUpdateUserName
                , value_ $ Model._userName m
                ]
            , button_
                [ onClick $ Model.Connect (Miso.fromMisoString $ _userName m)
                , class_ "button"
                ]
                [text "Connect"]
            ]
            -- Join session
        ]
    ]
  where
    signInUserNameId = "sign_in__user_name"

joinRoomView :: Model.Model -> View Model.Action
joinRoomView m =
  div_
    []
    [ div_
        []
        [ div_
            []
            -- Create Room
            ([ h2_ [] [text "Create Room"]
             , label_
                 [for_ createRoomNameId, class_ "label"]
                 [text "Room Name:"]
             , input_
                 [ id_ createRoomNameId
                 , class_ "label"
                 , onInput Model.CreateRoomUpdateName
                 , value_ $ Model._roomName m
                 ]
             , label_
                 [for_ createRoomStoryId, class_ "label"]
                 [text "Story Name:"]
             , input_
                 [ id_ createRoomStoryId
                 , class_ "label"
                 , onInput Model.CreateRoomUpdateStory
                 , value_ $ Model._roomStory m
                 ]
             , label_
                 [for_ createRoomPrivacyId, class_ "checkbox"]
                 [text "Private"]
             , input_
                 [ id_ createRoomPrivacyId
                 , class_ "checkbox"
                 , type_ "checkbox"
                 , onChecked $ \(Miso.Checked c) ->
                     Model.CreateRoomUpdatePrivacy c
                 , checked_ $ Model._roomPrivate m
                 ]
             ] ++
             cards ++
             [ button_
                 [onClick $ createRoom m, class_ "button"]
                 [text "Connect"]
             ])
        , div_
            []
            -- Join Room
            [ h2_ [] [text "Join Room"]
                -- Session ID
            , label_ [for_ joinRoomNameId, class_ "label"] [text "Room Id:"]
            , input_
                [ id_ joinRoomNameId
                , class_ "label"
                , onInput Model.JoinRoomUpdateId
                , value_ $ Model._roomId m
                ]
            , button_ [onClick $ joinRoom m, class_ "label"] [text "Connect"]
            ]
        ]
    ]
  where
    createRoomNameId = "create_room__name"
    createRoomPrivacyId = "create_room__privacy"
    createRoomStoryId = "create_room__story"
    joinRoomNameId = "join_room__id"
    createRoom :: Model.Model -> Model.Action
    createRoom m =
      let roomName = Miso.fromMisoString $ Model._roomName m
          story = Miso.fromMisoString $ Model._roomStory m
          deck = Model._roomDeck m
          private = Model._roomPrivate m
      in Model.CreateRoom roomName story deck private
    joinRoom :: Model.Model -> Model.Action
    joinRoom m =
      case readMaybe $ Miso.fromMisoString $ Model._roomId m of
        Just n  -> Model.JoinRoom n
        Nothing -> Model.NoOp
    cards :: [View Model.Action]
    cards =
      ([minBound .. maxBound] :: Deck) >>=
      (\card ->
         let cardName = Miso.toMisoString $ show card
             checked = elem card $ Model._roomDeck m
         in [ label_ [for_ cardName, class_ "checkbox"] [text cardName]
            , input_
                [ type_ "checkbox"
                , name_ cardName
                , id_ cardName
                , class_ "checkbox"
                , checked_ checked
                , onChecked $ \(Miso.Checked c) ->
                    Model.CreateRoomUpdateDeck card c
                ]
            ])

roomView :: Room.RoomId -> Model.Model -> View Action
roomView rid m =
    let rm = _room m in
    case rm of
        Nothing -> div_ [] [text "Whoopsie, something seems to have gone wrong"]
        Just rm' ->
            let
                rmName = Room._roomName rm'
                stry = Room._roomStory rm'
                prvt = Room._roomPrivate rm'
                dck = Room._roomDeck rm'
                crds = map (text . Miso.toMisoString . show) dck
            in
                div_
                    []
                    ([ h2_ [] [text $ Miso.toMisoString rmName]
                    , div_
                        []
                        [ p_ [] [text $ Miso.toMisoString stry]
                        , p_
                            []
                            [ text $
                            if prvt
                                then "Private"
                                else "Public"
                            ]
                        ]
                    ] ++
                    crds)


-- Handle 404 errors.
page404View :: View Model.Action
page404View =
  text
    "Whoopsy daisy... It looks like that page cannot be found. Sorry about that."
