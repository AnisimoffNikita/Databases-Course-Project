module Database.Queries.TestData (
    newTest
  , updateTest
  , getTest
  , deleteTest
  , getAllTests
) where

import Data.Text
import Database.MongoDB((=:))
import qualified Database.MongoDB as Mongo
import Data.Time (UTCTime)

import Model.TestData
import Database.Bson.TestData
import Database.Bson.Class


newTest :: TestData -> Mongo.Action IO TID
newTest test = do
  let test' = Mongo.exclude [tidColumn] (toDocument test)
  oid <- Mongo.insert collection test'
  return $ pack.show $ oid

updateTest :: TestData -> Mongo.Action IO ()
updateTest testData = Mongo.replace (selectColByTID $ tid testData) (toDocument testData)

getTest :: TID -> Mongo.Action IO (Maybe Mongo.Document)
getTest tid = Mongo.findOne (selectColByTID tid)

deleteTest :: TID -> Mongo.Action IO ()
deleteTest tid = Mongo.deleteOne (selectColByTID tid)

getAllTests :: Mongo.Action IO [Mongo.Document]
getAllTests = Mongo.rest =<< Mongo.find selectAllCol

selectAllCol :: Mongo.Select a => a
selectAllCol = Mongo.select [] collection

selectColByTID :: Mongo.Select a => Text -> a
selectColByTID tid = Mongo.select [tidColumn =: tid] collection
  where
    oid = read (unpack tid) :: Mongo.ObjectId

collection :: Mongo.Collection
collection = "tests"
