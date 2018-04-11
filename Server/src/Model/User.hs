module Model.User (
    Sex(..)
  , User(..)
) where


import Data.Text (Text)
import Data.Time

import Model.TestResult

data Sex
  = Male
  | Female
  deriving (Show, Read, Eq)

data User = User {
    username    :: Text
  , password    :: Text
  , email       :: Text
  , avatar      :: Text
  , results     :: [TestResult]
  , testKeys    :: [Text]
  , firstName   :: Maybe Text
  , secondName  :: Maybe Text
  , birthDay    :: Maybe UTCTime
  , sex         :: Maybe Sex
} deriving (Show, Eq)
