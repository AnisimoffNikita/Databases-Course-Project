module API.Types
  ( Login(..)
  , QuizPreview(..)
  , QuizQuestion(..)
  , ResponseResult
  , module Model.User
  , module Model.Quiz
  )
 where

import Data.Aeson
import Data.Time
import Data.Text (Text)
import GHC.Generics

import Model.Types
import Model.User
import Model.Quiz

data Login = Login
  { username :: String
  , password :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

data QuizPreview = QuizPreview
  { name :: Text
  , description :: Text
  , passingNumber :: Int
  } deriving (Eq, Show, Read, Generic)

instance ToJSON QuizPreview
instance FromJSON QuizPreview


data QuizQuestion = QuizQuestion
  { text :: Text
  , options :: QuestionOptions
  } deriving (Eq, Show, Read, Generic)

instance ToJSON QuizQuestion
instance FromJSON QuizQuestion


data ResponseResult a = ResponseError
  { code :: Int
  , message :: Text
  }
  | ResponseOk
  { response :: a
  } deriving (Eq, Show, Read, Generic)

instance ToJSON a => ToJSON (ResponseResult a)
instance FromJSON a => FromJSON (ResponseResult a)


instance ToJWT User
instance FromJWT User