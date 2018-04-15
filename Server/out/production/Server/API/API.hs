module API.API where

import Data.Text(Text)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import qualified API.V1.API as V1
import Model.User

type API auths = "v1" :> V1.API auths

apiProxy :: Proxy (API '[Cookie])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy