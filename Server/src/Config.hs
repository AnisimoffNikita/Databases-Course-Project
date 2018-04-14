{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (unpack, pack)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)
import Control.Monad.Except (ExceptT, MonadError)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Database.Persist.MongoDB
import Servant (ServantErr, Handler)
import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (defaultConfig, configPath)
import System.Environment (lookupEnv)
import Network (PortID (PortNumber))


data State = State
  { books :: TVar [Book]
  }

type AppM = ReaderT State Handler



data Config = Config
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbUser :: String
  , dbPass :: String
  }

data Environment
    = Development
    | Production
    deriving (Eq, Show, Read)

makePool :: Config -> Environment -> IO ConnectionPool
makePool _ Development = createMongoDBPool "DevTest" "localhost" (PortNumber 27017) Nothing 5 5 60
makePool Config{..} env = createMongoDBPool name host port auth 5 5 60
  where
    name = pack dbName
    host = dbHost
    port = PortNumber (fromIntegral dbPort)
    auth = (Just $ MongoAuth (pack dbUser) (pack dbPass))

load :: IO Config
load = do
  void $ loadFile config
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOSTNAME"
  dbName <- fromMaybe "spreadsheets" <$> lookupEnv "DB_NAME"
  dbPort <- read <$> fromMaybe "27017" <$> lookupEnv "DB_PORT"
  dbUser <- fromMaybe "user" <$> lookupEnv "DB_PORT"
  dbPass <- fromMaybe "pass" <$> lookupEnv "DB_PORT"
  return Config{..}
  where
    config = defaultConfig { configPath = [".config"] }