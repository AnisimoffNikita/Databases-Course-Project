{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Utils where


import Data.Text (unpack, pack)
import Database.Persist
import Database.Persist.MongoDB
import Network (PortID (PortNumber))

import Config

makePool :: Config -> Environment -> IO ConnectionPool
makePool _ Development = createMongoDBPool "DevTest" "localhost" (PortNumber 27017) Nothing 5 5 60
makePool Config{..} env = createMongoDBPool name host port auth 5 5 60
  where
    name = pack dbName
    host = dbHost
    port = PortNumber (fromIntegral dbPort)
    auth = (Just $ MongoAuth (pack dbUser) (pack dbPass))
