{-# LANGUAGE TemplateHaskell #-}
module API.User.Types
where

import           Data.Aeson
import           Data.Aeson.TH (deriveJSON)
import           Data.List                      ( stripPrefix )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Char
import           GHC.Generics
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan
                                                ( )

import           Model.Model
import           Model.Types
import           Utils


data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 5} ''Login)

data Tokens = Tokens
  { tokensJwt :: Text
  } deriving (Eq, Show, Read, Generic)

-- instance ToJSON Tokens
-- instance FromJSON Tokens
$(deriveJSON defaultOptions ''Tokens)


data JWTData = JWTData
  { jwtUsername :: Text
  } deriving (Eq, Show, Read, Generic)

-- instance ToJSON JWTData
-- instance FromJSON JWTData
$(deriveJSON defaultOptions ''JWTData)
instance ToJWT JWTData
instance FromJWT JWTData


data UserRegister = UserRegister
  { registerUsername :: Text
  , registerEmail :: Text
  , registerPassword :: Text
  } deriving (Eq, Show, Read, Generic)


$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 8} ''UserRegister)

userRegisterToUser :: UserRegister -> User
userRegisterToUser UserRegister {..} = User registerUsername
                                            (hashMD5 registerPassword)
                                            registerEmail
                                            avatarDefault
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            []
                                            []

userRegisterToLogin :: UserRegister -> Login
userRegisterToLogin UserRegister {..} = Login registerUsername registerPassword


data Profile = Profile
  { profileUsername   :: Text
  , profileEmail      :: Text
  , profileAvatar     :: Text
  , profileFirstName  :: Maybe Text
  , profileSecondName :: Maybe Text
  , profileBirthday   :: Maybe UTCTime
  , profileGender     :: Maybe Gender
  } deriving (Eq, Show, Read, Generic)


userToProfile :: User -> Profile
userToProfile User {..} = Profile userUsername
                                  userEmail
                                  userAvatar
                                  userFirstName
                                  userSecondName
                                  userBirthday
                                  userGender

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 7} ''Profile)

-- instance ToJSON Profile where
--   toJSON = genericToJSON (optionsWithoutPrefix removeProfile)
--   toEncoding = genericToEncoding (optionsWithoutPrefix removeProfile)
-- instance FromJSON Profile where
--   parseJSON = genericParseJSON (optionsWithoutPrefix removeProfile)
