module Types where

import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Database.MongoDB (Pipe)
import Servant (Handler)
import Data.Text (Text)

data AppContext = AppContext {
    pipePool :: Pool Pipe
  , databaseName :: Text
}

type App = ReaderT AppContext Handler