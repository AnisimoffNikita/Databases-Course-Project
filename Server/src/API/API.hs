module API.API where

import Data.Text(Text)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import Model.User

type Unprotected =
       "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie]
                                      NoContent)
  :<|> "newUser"
    :> ReqBody '[JSON] User
    :> PostNoContent '[JSON] NoContent
  :<|> "confirmUser"
    :> ReqBody '[JSON] User
    :> PostNoContent '[JSON] NoContent


type Protected =
       "checkLogin"
    :> Post '[JSON] Bool
  :<|> "user"
    :> Post '[JSON] User
  :<|> "user"
    :> "update"
    :> Post '[JSON] User
--  :<|> "quiz"
--    :> "list"
--    :> Post '[JSON] User
--  :<|> "quiz"
--    :> ReqBody '[JSON] Int
--    :> Post '[JSON] User

type V1 auths = (Auth auths User :> Protected) :<|> Unprotected

type API auths = "v1" :> V1 auths

apiProxy :: Proxy (API '[Cookie])
apiProxy = Proxy