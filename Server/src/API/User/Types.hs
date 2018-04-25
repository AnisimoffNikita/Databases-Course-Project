module API.User.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Model.Model
import Utils

data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

data Tokens = Tokens
  { tokensJwt :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Tokens
instance FromJSON Tokens

data JWTData = JWTData
  { jwtUsername :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON JWTData
instance FromJSON JWTData
instance ToJWT JWTData
instance FromJWT JWTData


data UserRegister = UserRegister
  { registerUsername :: Text
  , registerEmail :: Text
  , registerPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON UserRegister
instance FromJSON UserRegister

userRegisterToUser :: UserRegister -> User
userRegisterToUser UserRegister{..} =
  User registerUsername (hashMD5 registerPassword)
    registerEmail avatarDefault
    Nothing Nothing Nothing Nothing [] []

userRegisterToLogin :: UserRegister -> Login
userRegisterToLogin UserRegister{..} =
  Login registerUsername registerPassword