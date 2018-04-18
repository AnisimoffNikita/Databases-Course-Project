module API.API where

import Data.Text(Text)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import Model.User


type Protected =
       "isLoggedIn"
    :> Post '[JSON] (Maybe Text)
  :<|> "user"
    :> ReqBody '[JSON] Text
    :> Post '[JSON] (Maybe User)

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
    :> ReqBody '[JSON] Text
    :> PostNoContent '[JSON] NoContent

type API auths =
       Auth auths User
    :> Protected
  :<|> Unprotected



apiProxy :: Proxy (API '[Cookie])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy