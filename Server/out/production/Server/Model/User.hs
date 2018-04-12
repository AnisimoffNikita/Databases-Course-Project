module Model.User (
    Sex(..)
  , User(..)
  , TestResult(..)
  , UID
) where

import Data.Text (Text)
import Data.Time
import Model.TestData (TID)

type UID = Text

data User = User {
    uid         :: UID
  , username    :: Text
  , password    :: Text
  , email       :: Text
  , avatar      :: Text
  , firstName   :: Maybe Text
  , secondName  :: Maybe Text
  , birthDay    :: Maybe UTCTime
  , sex         :: Maybe Sex
  , results     :: [TestResult]
  , tidList     :: [TID]
} deriving (Show, Eq)

data Sex
  = Male
  | Female
  deriving (Show, Read, Eq)

data TestResult = TestResult {
    testKey     :: Text
  , result      :: Text
  , passingDate :: UTCTime
} deriving (Show, Eq)