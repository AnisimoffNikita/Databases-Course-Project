module Config where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (defaultConfig, configPath)
import System.Environment (lookupEnv)

data Config = Config
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbAuth :: Bool
  , dbUser :: String
  , dbPass :: String
  }


data Environment
    = Development
    | Production
    deriving (Eq, Show, Read)

load :: Environment -> IO Config
load Development = return $ Config "localhost" 27017 "test" False "" ""
load Production = load'

load' :: IO Config
load' = do
  void $ loadFile config
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOSTNAME"
  dbName <- fromMaybe "spreadsheets" <$> lookupEnv "DB_NAME"
  dbPort <- read . fromMaybe "27017" <$> lookupEnv "DB_PORT"
  dbUser <- fromMaybe "user" <$> lookupEnv "DB_PORT"
  dbPass <- fromMaybe "pass" <$> lookupEnv "DB_PORT"
  dbAuth <- return True
  return Config{..}
  where
    config = defaultConfig { configPath = [".config"] }

