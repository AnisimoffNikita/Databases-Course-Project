{-# LANGUAGE TemplateHaskell #-}
module API.Types
 where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.Server.SetCookieOrphan ()

import Model.Types


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


data Role = Admin | NonAdmin deriving (Eq, Show, Read, Generic)

instance ToJSON Role
instance FromJSON Role

data ResponseResult a = ResponseError
  { code :: Int
  , message :: Text
  } | ResponseOk
  { response :: a
  } deriving (Eq, Show, Read, Generic)

-- instance ToJSON a => ToJSON (ResponseResult a)
-- instance FromJSON a => FromJSON (ResponseResult a)

responseError :: Int -> Text -> ResponseResult a
responseError = ResponseError

responseOk :: a -> ResponseResult a
responseOk = ResponseOk



$(deriveJSON defaultOptions
        { constructorTagModifier = map toLower . drop 8 } ''ResponseResult)