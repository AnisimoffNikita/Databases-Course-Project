module API.Types
  ( Login(..)
  , userRegisterToLogin
  , UserRegister(..)
  , userRegisterToUser
  , QuizPreview(..)
  , QuizQuestion(..)
  , ResponseResult
  , module Model.Model
  )
 where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Time
import Data.Text (Text)
import GHC.Generics
import Servant.API.ContentTypes
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Web.FormUrlEncoded

import Model.Types
import Model.Model
import Utils


data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

instance ToJWT Login
instance FromJWT Login

userRegisterToLogin :: UserRegister -> Login
userRegisterToLogin UserRegister{..} =
  Login registerUsername registerPassword


data UserRegister = UserRegister
  { registerUsername :: Text
  , registerEmail :: Text
  , registerPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON UserRegister
instance FromJSON UserRegister

instance ToJWT UserRegister
instance FromJWT UserRegister

instance ToForm UserRegister
instance FromForm UserRegister

userRegisterToUser :: UserRegister -> User
userRegisterToUser UserRegister{..} =
  User registerUsername (hashMD5 registerPassword)
    registerEmail avatarDefault
    Nothing Nothing Nothing Nothing [] []


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