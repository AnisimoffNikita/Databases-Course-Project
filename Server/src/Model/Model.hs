{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Model
   where

import Prelude hiding (concat)

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text, pack, concat)
import Data.Time (UTCTime)
import qualified Database.MongoDB as Driver 
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax

import Model.Types


share[mkPersist (mkPersistSettings (ConT ''MongoContext))] [persistLowerCase|
Question json
  text     Text
  answer   Text
  variants [Text]
  deriving Eq Read Show Generic

Quiz json
  name            Text
  description     Text
  creationDate    UTCTime
  passingNumber   Int
  questions       [Question]
  deriving        Eq Read Show Generic

QuizResult json
  quizid          QuizId
  result          Text
  passing         UTCTime
  deriving        Eq Read Show Generic

User
  username        Text
  password        ByteString
  email           Text
  avatar          Text
  firstName       Text Maybe
  secondName      Text Maybe
  birthday        UTCTime Maybe
  gender          Gender Maybe
  createdQuizzes  [QuizId]
  passedQuizzes   [QuizResult]
  UniqueUsername  username
  UniqueEmail     email
  deriving        Eq Read Show Generic
|]

createIndexes :: ConnectionPool -> IO ()
createIndexes pool = do 
  runMongoDBPoolDef createUserIndexes pool
  runMongoDBPoolDef createQuizIndexes pool

createUserIndexes :: MonadIO m =>  Action m ()
createUserIndexes = do
  Driver.ensureIndex $ (Driver.index "user" ["username" =: (1 :: Int)]) {Driver.iUnique = True}
  Driver.ensureIndex $ (Driver.index "user" ["email" =: (1 :: Int)]) {Driver.iUnique = True}


createQuizIndexes :: MonadIO m =>  Action m ()
createQuizIndexes = createTextIndex "quiz" "nameText" ["name","description"]


createTextIndex :: MonadIO m =>  Driver.Collection -> Text -> [Driver.Label] -> Action m ()
createTextIndex col name keys = do
    db <- Driver.thisDatabase
    let doc = [ "ns"   =: concat [db, ".", col]
              , "key"  =: [key =: ("text" :: String) | key <- keys]
              , "name" =: name
              , "default_language" =: ("russian" :: Text)
              ]
    Driver.insert_ "system.indexes" doc