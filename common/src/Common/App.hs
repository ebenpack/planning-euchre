{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.App
    ( Rooms
    , Users
    , App
    , newApp
    , rooms
    , users
    , _rooms
    , _users
    )
where

import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toEncoding)
                                                , genericParseJSON
                                                , genericToEncoding
                                                )
import qualified Data.IntMap.Strict            as M
import           GHC.Generics                   ( Generic )

import           Common.JSON                    ( jsonOptions )
import           Common.Room                    ( Room )
import           Common.User                    ( User )

type Rooms = M.IntMap Room

type Users = M.IntMap User

data App = App { _rooms :: M.IntMap Room
               , _users :: M.IntMap User } deriving (Generic, Show)


newApp :: App
newApp = App {_rooms = M.empty, _users = M.empty}

makeLenses ''App

instance ToJSON App where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON App where
    parseJSON = genericParseJSON jsonOptions
