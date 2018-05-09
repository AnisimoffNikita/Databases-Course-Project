module API.Quiz.API
where

import           Data.Text                      ( Text )
import           Servant
import Servant.Multipart

import           API.User.Types
import           API.Types
import Model.Model


type QuizAPI =
       "new"
    :> ReqBody '[JSON] Quiz
    :> Post '[JSON] NoContent
  :<|> "get"
    :> Post '[JSON] [QuizPreview]
  :<|> "get" :> "all"
    :> Post '[JSON] [QuizPreview]
  :<|> "remove"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] NoContent
  :<|> "get"
    :> Capture "id" Text
    :> Post '[JSON] QuizWithoutAnswers
  :<|> "result"
    :> Capture "id" Text
    :> ReqBody '[JSON] [Text]
    :> Post '[JSON] Text
