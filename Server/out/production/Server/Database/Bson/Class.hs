module Database.Bson.Class (
    FromDocument(fromDocument)
  , ToDocument(toDocument)
  , ToObjectId(toObjectId)
  , FromObjectId(fromObjectId)
) where

import qualified Data.Bson as Bson

class FromDocument a where
  fromDocument :: Bson.Document -> Maybe a

class ToDocument a where
  toDocument :: a -> Bson.Document

class ToObjectId a where
  toObjectId :: a -> Bson.ObjectId

class FromObjectId a where
  fromObjectId :: Bson.ObjectId -> a