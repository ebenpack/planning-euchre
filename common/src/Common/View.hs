{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Common.View where

import           Control.Lens       (anyOf, at, folded, (&), (^.), (^?), _1,
                                     _Just)
import qualified Data.IntMap.Strict as M
import           Data.Proxy         (Proxy (..))
import qualified Data.Text          as Text
import           Miso               (View)
import qualified Miso
import           Miso.Html
import qualified Miso.String        as Miso
import           Servant.API        ((:<|>) (..))
import           Text.Read          (readMaybe)

import           Common.Card        (Card)
import           Common.Deck        (Deck)
import           Common.Model       as Model
import           Common.Room        as Room
import           Common.Routes      as Routes
import           Common.User        as User

viewModel :: Model.Model -> View Model.Action
viewModel model = section_
    [class_ "section"]
    [ div_
        [class_ "container"]
        [view]]
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
                [ onClick $ Model.Connect (Miso.fromMisoString $ Model._userName m)
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
    createRoom m' =
      let roomName' = Miso.fromMisoString $ Model._roomName m'
          story = Miso.fromMisoString $ Model._roomStory m'
          deck = Model._roomDeck m'
          private = Model._roomPrivate m'
      in Model.CreateRoom roomName' story deck private
    joinRoom :: Model.Model -> Model.Action
    joinRoom m' =
      case readMaybe $ Miso.fromMisoString $ Model._roomId m' of
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


votingView :: Model.Model -> Room.Room -> [View Action]
votingView m rm =
    let
        vt = _vote m
    in
        roomHeader_ m rm ++
        cardView m rm [""] vt True

resultsView :: Model.Model -> Room.Room -> [View Action]
resultsView m rm =
    let
        vt = _vote m
        usrs = (Room._roomUsers rm)
        uid = m ^. Model.userId
        rid = m ^? Model.room . _Just . Room.roomId
        userOwnsRoom = (m ^? Model.room . _Just . Room.roomOwner) == uid
        newStory =
            if userOwnsRoom
            then
                [ label_ [for_ "new_story", class_ "label"] [text "New Story:"]
                , input_
                    [ id_ "new_story"
                    , class_ "input"
                    , onInput Model.CreateRoomUpdateStory
                    , value_ $ Model._roomStory m
                    ]
                , button_
                    [ onClick $ case rid of
                        Just rid' -> Model.CreateNewStory rid' (Miso.fromMisoString $ Model._roomStory m)
                        Nothing -> Model.NoOp
                    , class_ "button"]
                    [text "New story"]]
            else []
    in
        roomHeader_ m rm ++
        results usrs ++
        newStory ++
        cardView m rm [""] vt False
    where
        results usrs =
            map (\u ->
                let
                    usr = (fst . snd) u
                    crd = case (snd . snd) u of
                        Just c  -> (Text.pack . show) c
                        Nothing -> ""
                in
                    p_
                        []
                        [text $ Miso.toMisoString $ Text.concat [User._userName usr, " ", crd]]
                ) $ M.toList usrs

roomHeader_ :: Model.Model -> Room.Room -> [View Action]
roomHeader_ _ rm =
    let
        rmName = Room._roomName rm
        stry = Room._roomStory rm
        prvt = Room._roomPrivate rm
        usrs = Text.intercalate ", " $ map (User._userName . fst . snd) $ M.toList (Room._roomUsers rm)
    in
        [ h2_ [] [text $ Miso.toMisoString rmName]
        , div_
            []
            [ p_ [] [text $ Miso.toMisoString stry]
            , p_ [] [text $ Miso.toMisoString usrs]
            , p_
                []
                [ text $
                if prvt
                    then "Private"
                    else "Public"
                ]
            ]
        ]

roomView :: Room.RoomId -> Model.Model -> View Action
roomView _ m =
    case _room m of
        Nothing -> div_ [] [text "Whoopsie, something seems to have gone wrong"]
        Just rm' ->
            div_
                []
                (case Room._roomState rm' of
                    Room.Voting  -> votingView m rm'
                    Room.Results -> resultsView m rm')


cardView :: Model.Model -> Room.Room -> [String] -> Maybe Card -> Bool -> [View Action]
cardView _ rm cls vt votingOpen =
    let
        dck = Room._roomDeck rm
    in
        -- TODO: shut off voting when not voting
        [div_
            [class_ $ Miso.toMisoString $ unwords ("deck" : "is-multiline" : "is-mobile" : "columns" : cls)]
            (map (\c ->
                let match = case vt of
                        Just crd | crd == c -> True
                        _                   -> False
                    cardClass = Text.intercalate " " $ ["card"] ++ if match then ["selected"] else []
                    cardText = [text $ Miso.toMisoString $ show c]
                    clickHandler = if votingOpen then Model.Vote c else Model.NoOp
                in
                    div_
                        [ onClick clickHandler
                        , class_ "column is-one-quarter-desktop is-one-third-tablet is-half-mobile"]
                        [div_ [class_ $ Miso.toMisoString cardClass]
                            [ div_ [class_ "card-head"] cardText
                            , div_
                                [class_ "level"]
                                [div_
                                    [class_ "level-item"]
                                    cardText]
                            , div_ [class_ "card-foot has-text-right"] cardText
                            ]]) dck)]

-- Handle 404 errors.
page404View :: View Model.Action
page404View =
  text
    "Whoopsy daisy... It looks like that page cannot be found. Sorry about that."
