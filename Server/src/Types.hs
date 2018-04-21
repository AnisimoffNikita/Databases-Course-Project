{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist.MongoDB
import Servant (Handler)
import Servant.Auth.Server

data HandlerContext = HandlerContext
  { connectionPool  :: ConnectionPool
  , jwt         :: JWTSettings
  }

type AppM = ReaderT HandlerContext Handler

