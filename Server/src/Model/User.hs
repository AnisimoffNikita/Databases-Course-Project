{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model.User
   where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.MongoDB (ensureIndex, index, iUnique, iDropDups)
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax

import Utils.Types


share[mkPersist (mkPersistSettings (ConT ''MongoContext)) { mpsPrefixFields = False }] [persistLowerCase|
QuizResult
  testKey         Text
  result          Text
  passingDate     UTCTime
  deriving        Eq Read Show Generic
User
  username        Text
  password        Text
  email           Text
  avatar          Text
  firstName       Text Maybe
  secondName      Text Maybe
  birthDay        UTCTime Maybe
  sex             Int Maybe
  createdQuizzes  [ObjectId]
  passedQuizzes   [QuizResult]
  deriving        Eq Read Show Generic
|]

createUserIndexes :: MonadIO m =>  Action m ()
createUserIndexes = do
  ensureIndex $ (index "user" ["username" =: (1::Int)]) {iUnique = True}
  ensureIndex $ (index "user" ["email" =: (1::Int)]) {iUnique = True}


instance FromJSON QuizResult
instance ToJSON QuizResult

instance FromJSON User
instance ToJSON User