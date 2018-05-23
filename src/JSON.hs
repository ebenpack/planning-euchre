module JSON (jsonOptions) where

import           Data.Aeson    (Options)
import           Data.Aeson.TH (defaultOptions, fieldLabelModifier)

jsonOptions :: Options
jsonOptions = defaultOptions{fieldLabelModifier = drop 1}
