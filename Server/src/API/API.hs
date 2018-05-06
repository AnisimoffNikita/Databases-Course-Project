module API.API where

import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Utils.StaticFiles 

import API.User.API
import API.User.Types


type API auths =
       Auth auths JWTData
    :> ( "user"
      :> UserAPI)

      
type WholeAPI auths = API auths :<|> Raw

apiProxy :: Proxy (API '[JWT])
apiProxy = Proxy

wholeAPI :: Proxy (WholeAPI '[JWT])
wholeAPI = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy
