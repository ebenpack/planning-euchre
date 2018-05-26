{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module User (
      User(User)
    , UserName
    , UserId
    , _userName
    , _userId
    , userId
    , userName) where

import           Control.Lens (makeLenses)
import           Data.Aeson   (FromJSON (parseJSON), ToJSON (toEncoding),
                               genericParseJSON, genericToEncoding)
import qualified Data.Text    as Text
import           GHC.Generics (Generic)
import           JSON         (jsonOptions)

type UserName = Text.Text

type UserId = Int

data User = User { _userId   :: UserId
                 , _userName :: UserName } deriving (Show, Generic)


makeLenses ''User

instance ToJSON User where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON User where
    parseJSON = genericParseJSON jsonOptions
