{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Bson (Document, (=:), Val, cast', val)
import qualified Data.Bson as B

data TestData = TestData {
    tid           :: Text
  , name          :: Text
  , description   :: Text
  , creationDate  :: UTCTime
  , passingNumber :: Integer
  , question      :: Text
} deriving (Show, Eq)


