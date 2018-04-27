module API.User.Types
where

import           Data.Aeson
import           Data.List                      ( stripPrefix )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
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

removeLogin :: String -> String
removeLogin = removePrefix "login"


instance ToJSON Login where
  toEncoding = genericToEncoding (optionsWithoutPrefix removeLogin)

instance FromJSON Login where
  parseJSON = genericParseJSON (optionsWithoutPrefix removeLogin)

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

removeRegister :: String -> String
removeRegister = removePrefix "register"


instance ToJSON UserRegister where
  toEncoding = genericToEncoding (optionsWithoutPrefix removeRegister)
instance FromJSON UserRegister where
  parseJSON = genericParseJSON (optionsWithoutPrefix removeRegister)

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


data UserInfo = UserInfo
  { infoUsername   :: Text
  , infoEmail      :: Text
  , infoAvatar     :: Text
  , infoFirstName  :: Maybe Text
  , infoSecondName :: Maybe Text
  , infoBirthday   :: Maybe UTCTime
  , infoGender     :: Maybe Gender
  } deriving (Eq, Show, Read, Generic)


userToUserInfo :: User -> UserInfo
userToUserInfo User {..} = UserInfo userUsername
                                    userEmail
                                    userAvatar
                                    userFirstName
                                    userSecondName
                                    userBirthday
                                    userGender

removeInfo :: String -> String
removeInfo = removePrefix "info"

instance ToJSON UserInfo where
  toEncoding = genericToEncoding (optionsWithoutPrefix removeInfo)
instance FromJSON UserInfo where
  parseJSON = genericParseJSON (optionsWithoutPrefix removeInfo)


removePrefix :: String -> String -> String
removePrefix p s = fromMaybe s (stripPrefix p s)


optionsWithoutPrefix :: (String -> String) -> Options
optionsWithoutPrefix mod = defaultOptions { fieldLabelModifier = mod }
