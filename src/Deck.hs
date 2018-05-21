module Deck(Deck) where

import           Card (Card)

newtype Deck = Deck [Card] deriving (Show)
