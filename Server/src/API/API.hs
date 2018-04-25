module API.API where

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.User.API
import API.User.Types


type API auths =
       Auth auths JWTData
    :> ( "user"
      :> UserAPI)


apiProxy :: Proxy (API '[JWT])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy