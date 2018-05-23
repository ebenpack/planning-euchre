{-# LANGUAGE TemplateHaskell #-}
module Deck(Deck) where

import           Card          (Card)
import           Data.Aeson.TH (defaultOptions, deriveJSON)

newtype Deck = Deck [Card] deriving (Show)


deriveJSON defaultOptions ''Deck
