module API.Types
where

import           Elm                            ( ElmType )
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Servant.Auth.Server.SetCookieOrphan
                                                ( )

import           API.Utils
import           Model.Types


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

instance ElmType Role
instance ToJSON Role
instance FromJSON Role

data ResponseResult a = ResponseError
  { code :: Int
  , message :: Text
  } | ResponseOk
  { response :: a
  } deriving (Eq, Show, Read, Generic)

instance ElmType a => ElmType (ResponseResult a)
instance ToJSON a => ToJSON (ResponseResult a) where
  toJSON = genericToJSON (optionsWithoutPrefix 8)
  toEncoding = genericToEncoding (optionsWithoutPrefix 8)
instance FromJSON a => FromJSON (ResponseResult a) where
  parseJSON = genericParseJSON (optionsWithoutPrefix 8)


responseError :: Int -> Text -> ResponseResult a
responseError = ResponseError

responseOk :: a -> ResponseResult a
responseOk = ResponseOk