{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model.Quiz
  (
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics
import Language.Haskell.TH.Syntax

share [mkPersist (mkPersistSettings (ConT ''MongoContext))] [persistLowerCase|
Quiz
  name          Text
  description   Text
  creationDate  UTCTime
  passingNumber Int
  question      Text
  deriving Eq Read Show Generic
|]

instance FromJSON Quiz
instance ToJSON Quiz