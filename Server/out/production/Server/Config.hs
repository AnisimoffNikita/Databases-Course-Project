module Config where

import Control.Monad (void)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)


data Config = Config
  { port    :: Int
  , dbHost  :: String
  , dbPort  :: Int
  , dbName  :: String
  } deriving (Show)
