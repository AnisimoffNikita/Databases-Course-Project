module API.Types where

import Data.Aeson
import Data.Time
import Data.Text (Text)
import GHC.Generics

import Model.User(TestResult, User, Sex)

instance ToJSON TestResult

instance ToJSON User

data UserRegister = UserRegister
  { username    :: Text
  , password    :: Text
  , email       :: Text
  , firstName   :: Maybe Text
  , secondName  :: Maybe Text
  , sex         :: Maybe Sex
  , birthday    :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON UserRegister

