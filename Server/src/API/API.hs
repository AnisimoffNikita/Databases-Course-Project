module API.API where

import Data.Text(Text)
import Data.ByteString (ByteString)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.Types
import Model.Model

type UserAPI =
       "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Tokens
  :<|> "new"
    :> ReqBody '[JSON] UserRegister
    :> Post '[JSON] Tokens
  :<|> "username"
    :> Post '[JSON] (Maybe Text)


type API auths =
       Auth auths JWTData
    :> ( "user"
      :> UserAPI)


apiProxy :: Proxy (API '[JWT])
apiProxy = Proxy

type AppContextType = '[CookieSettings, JWTSettings]

contextProxy :: Proxy AppContextType
contextProxy = Proxy