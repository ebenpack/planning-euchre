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

import           Control.Lens  (makeLenses)
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Text     as Text
import           JSON          (jsonOptions)

type UserName = Text.Text

type UserId = Int

data User = User { _userId   :: UserId
                 , _userName :: UserName }

instance Show User where
    show usr = "User { userId=\""
        ++ (show $ _userId usr)
        ++ "\", userName=\""
        ++ (show $ _userName usr)
        ++ "\" }"

makeLenses ''User

deriveJSON jsonOptions ''User
