{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.Bson.TestResult where

import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson

import Model.TestResult
import Database.Bson.Class

instance ToDocument TestResult where
  toDocument TestResult{..} =
    [ "testKey"     =: testKey
    , "result"      =: result
    , "passingDate" =: passingDate
    ]

instance FromDocument TestResult where
  fromDocument document =
    TestResult
      <$> Bson.lookup "username" document
      <*> Bson.lookup "result" document
      <*> Bson.lookup "passingDate" document