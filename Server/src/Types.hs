{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.Reader (ReaderT)
import Database.Persist.MongoDB
import Servant (Handler)

data AppContext = AppContext
  { connection :: ConnectionPool
  }

type AppM = ReaderT AppContext Handler

