module API.Types
where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import           GHC.Generics
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan

import           API.Utils
import           Model.Types
import           Model.Model


data QuizPreview = QuizPreview
  { name :: Text
  , description :: Text
  , passingNumber :: Int
  , id :: Text
  } deriving (Eq, Show, Read, Generic)

instance ToJSON QuizPreview
instance FromJSON QuizPreview

quizToPreview :: Text -> Quiz -> QuizPreview
quizToPreview id Quiz{..} = QuizPreview quizName quizDescription quizPassingNumber id

data QuestionWithoutAnswer = QuestionWithoutAnswer
  { text :: Text
  , variants :: [Text]
  } deriving (Eq, Show, Read, Generic)

questionWithoutAnswer :: Question -> QuestionWithoutAnswer
questionWithoutAnswer Question{..} = QuestionWithoutAnswer questionText questionVariants

instance ToJSON QuestionWithoutAnswer
instance FromJSON QuestionWithoutAnswer

data QuizWithoutAnswers = QuizWithoutAnswers
  { dataName :: Text
  , dataDescription :: Text
  , dataQuestions :: [QuestionWithoutAnswer]
  } deriving (Eq, Show, Read, Generic)

quizWithoutAnswers :: Quiz -> QuizWithoutAnswers
quizWithoutAnswers Quiz{..} = QuizWithoutAnswers quizName quizDescription (map questionWithoutAnswer quizQuestions)

instance ToJSON QuizWithoutAnswers where
  toJSON = genericToJSON (optionsWithoutPrefix 4)
  toEncoding = genericToEncoding (optionsWithoutPrefix 4)
instance FromJSON QuizWithoutAnswers where
  parseJSON = genericParseJSON (optionsWithoutPrefix 4)

data Role = Admin | NonAdmin deriving (Eq, Show, Read, Generic)

instance ToJSON Role
instance FromJSON Role

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
