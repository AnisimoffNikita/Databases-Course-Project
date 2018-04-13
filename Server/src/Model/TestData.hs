module Model.TestData
  ( TestData(..)
  , tidColumn
  , nameColumn
  , descriptionColumn
  , creationDateColumn
  , passingNumberColumn
  , questionColumn
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Database.Bson.Class
import Model.Types

data TestData = TestData
  { tid           :: ID
  , name          :: Text
  , description   :: Text
  , creationDate  :: UTCTime
  , passingNumber :: Integer
  , question      :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON TestData
instance ToJSON TestData

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
      <$> fmap fromObjectId (Bson.lookup tidColumn document)
      <*> Bson.lookup nameColumn document
      <*> Bson.lookup descriptionColumn document
      <*> Bson.lookup creationDateColumn document
      <*> Bson.lookup passingNumberColumn document
      <*> Bson.lookup questionColumn document

tidColumn           = "_id"           :: Bson.Label
nameColumn          = "name"          :: Bson.Label
descriptionColumn   = "description"   :: Bson.Label
creationDateColumn  = "creationDate"  :: Bson.Label
passingNumberColumn = "passingNumber" :: Bson.Label
questionColumn      = "question"      :: Bson.Label