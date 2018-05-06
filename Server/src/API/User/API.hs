module API.User.API
where

import           Data.Text                      ( Text )
import           Servant

import           API.User.Types

type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Tokens
  :<|> "register"
    :> ReqBody '[JSON] UserRegister
    :> Post '[JSON] Tokens
  :<|> "username"
    :> Post '[JSON] Text
  :<|> "profile"
    :> Post '[JSON] Profile
  :<|> "edit" 
    :> ( "username" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] NoContent
    :<|> "password" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] NoContent 
    :<|> "email" 
      :> ReqBody '[JSON] Text 
      :> Post '[JSON] NoContent
    :<|> "avatar" 
      :> Post '[JSON] Text 
    :<|> "profile" 
      :> ReqBody '[JSON] UserInfo 
      :> Post '[JSON] NoContent 
       )
