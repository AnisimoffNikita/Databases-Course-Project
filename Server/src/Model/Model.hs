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

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.MongoDB (index, iUnique, ensureIndex)
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax

import Model.Types


share[mkPersist (mkPersistSettings (ConT ''MongoContext))] [persistLowerCase|
Question json
  text    Text
  option  QuestionOptions
  answer  QuestionAnswer
  deriving Eq Read Show Generic

Quiz json
  name            Text
  description     Text
  creationDate    UTCTime
  passingNumber   Int
  questions       [Question]
  deriving        Eq Read Show Generic

QuizResult json
  testKey         Text
  result          Text
  passingDate     UTCTime
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

createUserIndexes :: MonadIO m =>  Action m ()
createUserIndexes = do
  ensureIndex $ (index "user" ["username" =: (1 :: Int)]) {iUnique = True}
  ensureIndex $ (index "user" ["email" =: (1 :: Int)]) {iUnique = True}


createQuizIndexes :: MonadIO m =>  Action m ()
createQuizIndexes = return ()