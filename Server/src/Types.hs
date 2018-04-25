{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist.MongoDB (ConnectionPool)
import Servant (Handler)
import Servant.Auth.Server (JWTSettings)

data HandlerContext = HandlerContext
  { connectionPool  :: ConnectionPool
  , jwt             :: JWTSettings
  }

type AppM = ReaderT HandlerContext Handler

