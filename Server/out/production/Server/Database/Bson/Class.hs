module Database.Bson.Class where

import qualified Data.Bson as Bson

class FromDocument a where
  fromDocument :: Bson.Document -> Maybe a

class ToDocument a where
  toDocument :: a -> Bson.Document
