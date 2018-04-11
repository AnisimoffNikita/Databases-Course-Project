{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.Bson.TestData where

import Data.Text (Text, pack)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Database.Bson.Class

import Model.TestData

instance ToDocument TestData where
  toDocument TestData{..} =
    [ "_id"           =: _id
    , "name"          =: name
    , "description"   =: description
    , "creationDate"  =: creationDate
    , "passingNumber" =: passingNumber
    , "question"      =: question
    ]


instance FromDocument TestData where
  fromDocument document =
    TestData
      <$> Bson.lookup "_id" document
      <*> Bson.lookup "name" document
      <*> Bson.lookup "description" document
      <*> Bson.lookup "creationDate" document
      <*> Bson.lookup "passingNumber" document
      <*> Bson.lookup "question" document
