module Common.JSON (jsonOptions) where

import           Data.Aeson.TH    (defaultOptions, fieldLabelModifier)
import           Data.Aeson.Types (Options)

jsonOptions :: Options
jsonOptions = defaultOptions{fieldLabelModifier = drop 1}
