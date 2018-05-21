{-# LANGUAGE TemplateHaskell #-}
module User (
      User(User)
    , UserName
    , UserId
    , _userName
    , _userId
    , userId
    , userName
    , connection) where

import           Control.Lens       (makeLensesFor)
import qualified Data.Text          as Text
import qualified Network.WebSockets as WS

type UserName = Text.Text

type UserId = Int

data User = User { userId     :: UserId
                 , userName   :: UserName
                 , connection :: WS.Connection }

instance Show User where
    show usr = "User { userId=\""
        ++ (show $ userId usr)
        ++ "\", userName=\""
        ++ (show $ userName usr)
        ++ "\" }"

makeLensesFor [("userName", "_userName"), ("userId", "_userId")] ''User
