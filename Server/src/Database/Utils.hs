module Database.Utils where


import Data.Text (unpack, pack)
import Database.Persist
import Database.Persist.MongoDB
import Network (PortID (PortNumber))

import Config

makePool :: Config  -> IO ConnectionPool
makePool Config{..} = createMongoDBPool name host port auth 5 5 60
  where
    name = pack dbName
    host = dbHost
    port = PortNumber (fromIntegral dbPort)
    auth = if dbAuth then (Just $ MongoAuth (pack dbUser) (pack dbPass)) else Nothing
