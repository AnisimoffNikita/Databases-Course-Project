{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist.MongoDB
import Servant (Handler)
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

data HandlerContext = HandlerContext
  { connectionPool  :: ConnectionPool
  , cookie      :: CookieSettings
  , jwt         :: JWTSettings
  }

type AppM = ReaderT HandlerContext Handler

