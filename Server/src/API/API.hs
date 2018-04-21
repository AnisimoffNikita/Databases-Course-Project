module API.API where

import Data.Text(Text)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import Model.Model

type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie
                                       ]
                                       NoContent)
  :<|> "new"
    :> ReqBody '[JSON] UserRegister
    :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                       , Header "Set-Cookie" SetCookie
                                       ]
                                       NoContent)
  :<|> "username"
    :> Post '[JSON] (Maybe Text)


type API auths =
       Auth auths Login
    :> ( "user"
      :> UserAPI)


apiProxy :: Proxy (API '[Cookie])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy