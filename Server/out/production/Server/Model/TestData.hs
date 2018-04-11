{-# LANGUAGE OverloadedStrings #-}
module Model.TestData where

import Data.Text (Text, pack)
import Data.Time
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson

data TestData = TestData {
    _id           :: Text
  , name          :: Text
  , description   :: Text
  , creationDate  :: UTCTime
  , passingNumber :: Integer
  , question      :: Text
} deriving (Show, Eq)

