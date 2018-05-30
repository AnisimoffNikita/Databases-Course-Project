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
  :<|> "get" :> "user"
    :> Get '[JSON] [QuizPreview]
  :<|> "get" :> "all"
    :> Get '[JSON] [QuizPreview]
  :<|> "get" :> "passed"
    :> Get '[JSON] [QuizPreviewResult]
  :<|> "get"
    :> Capture "id" Text
    :> Get '[JSON] QuizWithoutAnswers
  :<|> "search"
    :> Capture "query" Text
    :> Get '[JSON] [QuizPreview]
  :<|> "remove"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] NoContent
  :<|> "result"
    :> Capture "id" Text
    :> ReqBody '[JSON] [Text]
    :> Post '[JSON] Text
