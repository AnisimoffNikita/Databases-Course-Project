module App where

import Network.Wai
import Network.Wai.Handler.Warp
import Model.User
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (lift)
import Config
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Server
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH

import API.API
import API.Handlers

import Config
import Types

nt :: AppContext -> AppM a -> Handler a
nt s x = runReaderT x s

app :: CookieSettings -> JWTSettings -> AppContext -> Application
app cookieCfg jwtCfg s =
  serveWithContext apiProxy cfg $
                   hoistServerWithContext apiProxy contextProxy (nt s) (handlers cookieCfg jwtCfg)
  where
    cfg = cookieCfg :. jwtCfg :. EmptyContext
    contextProxy = Proxy :: Proxy (CookieSettings ': JWTSettings ': '[])

