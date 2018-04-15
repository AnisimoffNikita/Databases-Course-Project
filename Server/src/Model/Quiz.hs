{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Quiz
  (
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.MongoDB (ensureIndex, index, iUnique, iDropDups)
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics
import Language.Haskell.TH.Syntax

import Model.Types

share [mkPersist (mkPersistSettings (ConT ''MongoContext))] [persistLowerCase|
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
  question        Question
  deriving Eq Read Show Generic
|]


createQuizIndexes :: MonadIO m =>  Action m ()
createQuizIndexes = return ()
