module API.User.Types
where

import           Elm                            ( ElmType )
import           Data.Aeson
import           Data.List                      ( stripPrefix )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.Char
import           GHC.Generics
import           Servant.Auth.Server

import           API.Utils
import           Model.Model
import           Model.Types
import           Utils

data Login = Login
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login where
  toJSON = genericToJSON (optionsWithoutPrefix 5)
  toEncoding = genericToEncoding (optionsWithoutPrefix 5)
instance FromJSON Login where
  parseJSON = genericParseJSON (optionsWithoutPrefix 5)


data UserRegister = UserRegister
  { registerUsername :: Text
  , registerEmail :: Text
  , registerPassword :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON UserRegister where
  toJSON = genericToJSON (optionsWithoutPrefix 8)
  toEncoding = genericToEncoding (optionsWithoutPrefix 8)
instance FromJSON UserRegister where
  parseJSON = genericParseJSON (optionsWithoutPrefix 8)

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

instance ToJSON Profile where
  toJSON = genericToJSON (optionsWithoutPrefix 7)
  toEncoding = genericToEncoding (optionsWithoutPrefix 7)
instance FromJSON Profile where
  parseJSON = genericParseJSON (optionsWithoutPrefix 7)

data UserInfo = UserInfo
  { infoFirstName  :: Maybe Text
  , infoSecondName :: Maybe Text
  , infoBirthday   :: Maybe UTCTime
  , infoGender     :: Maybe Gender
  } deriving (Eq, Show, Read, Generic)


userToInfo :: User -> UserInfo
userToInfo User {..} = UserInfo userFirstName
                                userSecondName
                                userBirthday
                                userGender

instance ToJSON UserInfo where
  toJSON = genericToJSON (optionsWithoutPrefix 4)
  toEncoding = genericToEncoding (optionsWithoutPrefix 4)
instance FromJSON UserInfo where
  parseJSON = genericParseJSON (optionsWithoutPrefix 4)
