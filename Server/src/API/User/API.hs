module API.User.API
where

import           Data.Text                      ( Text )
import           Servant
import Servant.Multipart

import           API.User.Types
import           API.Types

type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Tokens
  :<|> "register"
    :> ReqBody '[JSON] UserRegister
    :> Post '[JSON] Tokens
  :<|> "username"
    :> Get '[JSON] Text
  :<|> "profile"
    :> Get '[JSON] Profile
  :<|> "edit" 
    :> ( "username" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] Tokens
    :<|> "password" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] NoContent 
    :<|> "email" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] NoContent
    :<|> "avatar" 
      :> MultipartForm Mem (MultipartData Mem) 
      :> Post '[JSON] Text 
    :<|> "profile" 
      :> ReqBody '[JSON] UserInfo 
      :> Post '[JSON] NoContent 
       )
