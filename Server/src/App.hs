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
import API.Handler

import Database.Utils
import Config
import Types

nt :: HandlerContext -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Context AppContextType -> HandlerContext -> Application
app context s =
  serveWithContext apiProxy context $
                   hoistServerWithContext apiProxy contextProxy (nt s) handler


startApp :: IO ()
startApp = do
  myKey <- generateKey
  let
    jwtCfg = defaultJWTSettings myKey
    cookieCfg = defaultCookieSettings
    appContext = cookieCfg :. jwtCfg :. EmptyContext

  dbConfig <- load Development
  pool <- makePool dbConfig
  let
    handlerContext = HandlerContext pool cookieCfg jwtCfg

  run 7249 $ app appContext handlerContext
