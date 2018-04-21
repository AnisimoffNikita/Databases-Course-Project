module API.API where

import Data.Text(Text)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import Model.Model


type Protected =
       "getLoggedIn"
    :> Post '[JSON] (Maybe Text)
  :<|> "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie
                                       ]
                                       NoContent)
  :<|> "newUser"
    :> ReqBody '[FormUrlEncoded] UserRegister
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie
                                       ]
                                       NoContent)
  :<|> "confirmUser"
    :> ReqBody '[JSON] Text
    :> PostNoContent '[JSON] NoContent

type API auths =
       Auth auths Login
    :> Protected


apiProxy :: Proxy (API '[Cookie])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy