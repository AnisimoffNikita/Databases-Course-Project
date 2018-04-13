{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model.User
  (
  ) where


import Data.Aeson
import Data.Text
import Data.Time

import GHC.Generics
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Control.Monad.Reader
import Language.Haskell.TH.Syntax

import Database.Persist.TH

share [mkPersist (mkPersistSettings (ConT ''MongoContext)), mkMigrate "migrateAll"] [persistLowerCase|
TestResult
  testKey     Text
  result      Text
  passingDate UTCTime
  deriving Eq Read Show Generic
User
  username    Text
  password    Text
  email       Text
  avatar      Text
  firstName   Text Maybe
  secondName  Text Maybe
  birthDay    UTCTime Maybe
  sex         Int Maybe
  Email       email
  Username    username
  deriving Eq Read Show Generic
|]


instance FromJSON User
instance ToJSON User