module Database.Utils where


import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (pack)
import Database.MongoDB
import Types
import Config


mkPool ∷ Config -> IO (Pool Pipe)
mkPool config = createPool pipe close 5 60 5
  where
    pipe = connect host
    host = Host (dbHost config) port
    port = PortNumber (fromIntegral $ dbPort config)

runDb ∷ Action IO a -> App a
runDb op = do
  pool <- asks pipePool
  name <- asks databaseName
  liftIO $ withResource pool $
    \pipe → access pipe master (pack name) op