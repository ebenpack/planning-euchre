{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Common.View where

import           Control.Lens       ((^.), (^?), _Just)
import qualified Data.IntMap.Strict as M
import           Data.Maybe         (isJust)
import           Data.Proxy         (Proxy (Proxy))
import qualified Data.Text          as Text
import           Miso               (View)
import qualified Miso
import           Miso.Html          (button_, checked_, class_, div_, for_, h2_,
                                     id_, input_, label_, name_, onChecked,
                                     onClick, onInput, p_, section_, text,
                                     type_, value_)
import qualified Miso.String        as Miso
import           Servant.API        ((:<|>) ((:<|>)))
import           Text.Read          (readMaybe)

import           Common.Card        (Card)
import           Common.Deck        (Deck)
import           Common.Model       as Model (Action (CloseVote, Connect, CreateNewStory, CreateRoom, CreateRoomUpdateDeck, CreateRoomUpdateName, CreateRoomUpdatePrivacy, CreateRoomUpdateStory, JoinRoom, JoinRoomUpdateId, NoOp, SignInUpdateUserName, Vote, VotingClosed),
                                              Model, room, roomId, userId,
                                              _room, _roomDeck, _roomId,
                                              _roomName, _roomPrivate,
                                              _roomStory, _uri, _userName,
                                              _vote)
import           Common.Room        as Room (Room, RoomId, RoomState (VotingClosed, VotingComplete, VotingOpen),
                                             roomId, roomOwner, _roomDeck,
                                             _roomName, _roomPrivate,
                                             _roomState, _roomStory, _roomUsers)
import           Common.Routes      as Routes (ViewRoutes)
import           Common.User        as User (User, userId, _userName)

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


votingView :: Model.Model -> Room.Room -> Bool -> [View Action]
votingView m rm complt =
    let
        vt = _vote m
        uid = m ^. Model.userId
        rid = m ^? Model.room . _Just . Room.roomId
        usrs = (Room._roomUsers rm)
        userOwnsRoom = isJust uid && (m ^? Model.room . _Just . Room.roomOwner) == uid
        closeVote = if complt && userOwnsRoom then [
            button_
                [ onClick $ case rid of
                    Just rid' -> Model.CloseVote rid'
                    Nothing   -> Model.NoOp
                , class_ "button"]
                [text "New story"]] else []
    in
        roomHeader_ m rm (roomUsers usrs) ++
        closeVote ++
        cardView m rm [""] vt True
    where
        roomUsers :: Show a => M.IntMap (User, Maybe a) -> [View Action]
        roomUsers usrs =
            [p_ [] [text "Users:"]] ++
            (map (\u ->
                let
                    usr = (fst . snd) u
                in
                    p_
                        []
                        [text $ Miso.toMisoString $ User._userName usr]
                ) $ M.toList usrs)

resultsView :: Model.Model -> Room.Room -> [View Action]
resultsView m rm =
    let
        vt = _vote m
        usrs = (Room._roomUsers rm)
        uid = m ^. Model.userId
        rid = m ^? Model.room . _Just . Room.roomId
        userOwnsRoom = isJust uid && (m ^? Model.room . _Just . Room.roomOwner) == uid
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
        roomHeader_ m rm (results usrs) ++
        newStory ++
        cardView m rm [""] vt False
    where
        results :: Show a => M.IntMap (User, Maybe a) -> [View Action]
        results usrs =
            [p_ [] [text "Results:"]] ++
            (map (\u ->
                let
                    usr = (fst . snd) u
                    crd = case (snd . snd) u of
                        Just c  -> (Text.pack . show) c
                        Nothing -> ""
                in
                    p_
                        []
                        [text $ Miso.toMisoString $ Text.concat [User._userName usr, " ", crd]]
                ) $ M.toList usrs)

roomHeader_ :: Model.Model -> Room.Room -> [View Action] -> [View Action]
roomHeader_ _ rm results =
    let
        rmName = Text.concat ["Room: ", Room._roomName rm]
        stry = Text.concat ["Story: ", Room._roomStory rm]
        prvt = Room._roomPrivate rm
        usrs = Text.intercalate ", " $ map (User._userName . fst . snd) $ M.toList (Room._roomUsers rm)
    in
        [
            div_ [class_ "columns is-mobile"] [
                div_
                    [class_ "column"]
                    [ h2_ [] [text $ Miso.toMisoString rmName]
                    , h2_ [] [text $ Miso.toMisoString stry]
                    , text $
                        if prvt
                            then "Private"
                            else "Public"
                    ]
                , div_
                    [class_ "column"] results]]

roomView :: Room.RoomId -> Model.Model -> View Action
roomView _ m =
    case _room m of
        Nothing -> div_ [] [text "Whoopsie, something seems to have gone wrong"]
        Just rm' ->
            div_
                []
                (case Room._roomState rm' of
                    Room.VotingOpen     -> votingView m rm' False
                    Room.VotingComplete -> votingView m rm' True
                    Room.VotingClosed   -> resultsView m rm')


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
