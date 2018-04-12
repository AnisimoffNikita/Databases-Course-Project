module Types where

import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Database.MongoDB (Pipe)
import Servant (Handler)

data AppContext = AppContext {
    pipePool :: Pool Pipe
  , databaseName :: String
}

type App = ReaderT AppContext Handler