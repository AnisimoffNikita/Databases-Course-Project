{-# LANGUAGE TypeSynonymInstances #-}
module Model.Types where

import Data.Text(Text, pack, unpack)
import Database.Bson.Class
import Data.Bson (ObjectId)

type ID = Text

instance ToObjectId ID where
  toObjectId uid = read (unpack uid) :: ObjectId

instance FromObjectId ID where
  fromObjectId = pack . show