module Database.Queries
  ( module Database.Queries.User
  , module Database.Queries.TestData
  ) where

import Database.Queries.User
import Database.Queries.TestData

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.MongoDB (runMongoDBPool)
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)

import App
import Config

runDb :: AppM
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool