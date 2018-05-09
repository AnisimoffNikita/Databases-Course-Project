{-# LANGUAGE TemplateHaskell #-}
module Model.Types where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Text
import Database.Persist.MongoDB
import Database.Persist.TH
import GHC.Generics

instance Aeson.ToJSON ObjectId where
  toJSON = Aeson.toJSON . pack . show
  

instance Aeson.FromJSON ObjectId where
  parseJSON (Aeson.String v) =
    case readed of
      Just oid -> return oid
      Nothing -> mzero
    where
      readed = readMayObjectId $ v
  parseJSON _ = mzero


data Gender =
    Male
  | Female
  deriving (Show, Read, Eq, Generic)
derivePersistField "Gender"

instance Aeson.ToJSON Gender
instance Aeson.FromJSON Gender

data QuestionOptions =
    CheckOption Int
  | RadioOption Int
  | LineOption
  deriving (Show, Read, Eq, Generic)
derivePersistField "QuestionOptions"

instance Aeson.ToJSON QuestionOptions
instance Aeson.FromJSON QuestionOptions

data QuestionAnswer =
    CheckAnswer Int
  | RadioAnswer Int
  | LineAnswer Text
  deriving (Show, Read, Eq, Generic)
derivePersistField "QuestionAnswer"

instance Aeson.ToJSON QuestionAnswer
instance Aeson.FromJSON QuestionAnswer
