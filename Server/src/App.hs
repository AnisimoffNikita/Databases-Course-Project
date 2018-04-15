module App where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
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
  dbConfig <- load Development
  pool <- makePool dbConfig
  let
    jwtCfg = defaultJWTSettings myKey
    cookieCfg = defaultCookieSettings
    appContext = cookieCfg :. jwtCfg :. EmptyContext
    handlerContext = HandlerContext pool cookieCfg jwtCfg

  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings $ app appContext handlerContext
