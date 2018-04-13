{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model.User
  ( User(..)
  , TestResult(..)
  ) where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics
import Language.Haskell.TH.Syntax
import Utils.Types
import Control.Lens

share [mkPersist (mkPersistSettings (ConT ''MongoContext)), mkMigrate "migrateAll"] [persistLowerCase|
TestResult
  testKey       Text
  result        Text
  passingDate   UTCTime
  deriving      Eq Read Show Generic
User
  username      Text
  password      Text
  email         Text
  avatar        Text
  firstName     Text Maybe
  secondName    Text Maybe
  birthDay      UTCTime Maybe
  sex           Int Maybe
  createdTests  [ObjectId]
  passedTests   [TestResult]
  Email         email
  Username      username
  deriving      Eq Read Show Generic
|]


instance Aeson.FromJSON TestResult
instance Aeson.ToJSON TestResult

instance Aeson.FromJSON User
instance Aeson.ToJSON User