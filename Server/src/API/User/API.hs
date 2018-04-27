module API.User.API
where

import           Data.Text                      ( Text )
import           Servant

import           API.User.Types
import           API.Types (ResponseResult)

type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] (ResponseResult Tokens)
  :<|> "new"
    :> ReqBody '[JSON] UserRegister
    :> Post '[JSON] (ResponseResult Tokens)
  :<|> "username"
    :> Post '[JSON] (ResponseResult Text)
  :<|> "profile"
    :> Post '[JSON] (ResponseResult Profile)
  :<|> "list"
    :> Post '[JSON] (ResponseResult [Profile])
