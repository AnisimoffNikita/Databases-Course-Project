module App where

import Network.Wai
import Network.Wai.Middleware.Cors (corsRequestHeaders, cors, simpleCorsResourcePolicy, corsOrigins )
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Control.Monad.Reader (runReaderT)
import Config
import Servant
import Servant.Auth.Server

import API.API
import API.Handler

import Database.Utils
import Types

import Data.List ( transpose )

f = id

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
    handlerContext = HandlerContext pool jwtCfg

  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings $ corsWithContentType $ app appContext handlerContext


corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        }
