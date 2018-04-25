module API.User.API where

import Data.Text(Text)
import Servant

import API.User.Types


type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Tokens
  :<|> "new"
    :> ReqBody '[JSON] UserRegister
    :> Post '[JSON] Tokens
  :<|> "username"
    :> Post '[JSON] (Maybe Text)
