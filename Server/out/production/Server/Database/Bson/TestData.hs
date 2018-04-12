{-# LANGUAGE TypeSynonymInstances #-}

module Database.Bson.TestData (
    tidColumn
  , nameColumn
  , descriptionColumn
  , creationDateColumn
  , passingNumberColumn
  , questionColumn
) where

import Data.Text (Text, pack, unpack)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Database.Bson.Class


import Model.TestData

instance ToDocument TestData where
  toDocument TestData{..} =
    [ tidColumn           =: tid
    , nameColumn          =: name
    , descriptionColumn   =: description
    , creationDateColumn  =: creationDate
    , passingNumberColumn =: passingNumber
    , questionColumn      =: question
    ]


instance FromDocument TestData where
  fromDocument document =
    TestData
      <$> fmap (pack . show) (Bson.lookup tidColumn document :: Maybe Bson.ObjectId)
      <*> Bson.lookup nameColumn document
      <*> Bson.lookup descriptionColumn document
      <*> Bson.lookup creationDateColumn document
      <*> Bson.lookup passingNumberColumn document
      <*> Bson.lookup questionColumn document

instance ToObjectId TID where
  toObjectId uid = read (unpack uid) :: Bson.ObjectId

instance FromObjectId TID where
  fromObjectId = pack . show

tidColumn           = "_id"           :: Bson.Label
nameColumn          = "name"          :: Bson.Label
descriptionColumn   = "description"   :: Bson.Label
creationDateColumn  = "creationDate"  :: Bson.Label
passingNumberColumn = "passingNumber" :: Bson.Label
questionColumn      = "question"      :: Bson.Label