module Config where

import Control.Monad (void)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Configuration.Dotenv.Scheme
import Configuration.Dotenv.Types (defaultConfig, configPath)

data Env = Prod | Test

data Config = Config
  { port :: Int
  , dbHost :: String
  , dbPort :: Int
  , dbName :: String
  } deriving (Show)

load :: Env -> IO Config
load env = do
  void $ loadSafeFile ".scheme.yml" config
  port   <- read <$> fromMaybe "3000" <$> lookupEnv "PORT"
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
  dbPort <- read <$> fromMaybe "27017" <$> lookupEnv "DB_PORT"
  dbName <- fromMaybe "testSystem" <$> lookupEnv "DB_NAME"
  return Config{..}
  where
    config = defaultConfig { configPath = [fileName] }
    fileName = ".env" ++ envPostfix env

envPostfix ∷ Env -> String
envPostfix Prod = ""
envPostfix Test = ".test"