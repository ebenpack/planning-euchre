{-# LANGUAGE DeriveGeneric #-}
module Deck (Deck(Deck)) where

import           Card         (Card)
import           Data.Aeson   (FromJSON, ToJSON (toEncoding), defaultOptions,
                               genericToEncoding)
import           GHC.Generics (Generic)

newtype Deck = Deck [Card] deriving (Show, Generic)

instance ToJSON Deck where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Deck
