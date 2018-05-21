{-# LANGUAGE TemplateHaskell #-}
module Room(RoomId,RoomName,Room,_roomName,_roomUsers,_roomStory,_roomDeck,_roomPrivate,_roomId) where

import           Control.Lens (makeLensesFor)
import qualified Data.Map     as Map
import           Deck         (Deck)
import           Story        (Story)
import           User         (User, UserId)

type RoomId = Int

type RoomName = String

data Room = Room { roomId  :: RoomId
                 , name    :: RoomName
                 , owner   :: UserId
                 , users   :: Map.Map UserId User
                 , story   :: Story
                 , deck    :: Deck
                 , private :: Bool } deriving (Show)

makeLensesFor [
      ("name", "_roomName")
    , ("users", "_roomUsers")
    , ("story", "_roomStory")
    , ("deck", "_roomDeck")
    , ("private", "_roomPrivate")
    , ("roomId", "_roomId")
    ] ''Room
