module Model.User
  ( Sex(..)
  , User(..)
  , TestResult(..)
  , uidColumn
  , usernameColumn
  , passwordColumn
  , emailColumn
  , avatarColumn
  , firstNameColumn
  , secondNameColumn
  , birthdayColumn
  , sexColumn
  , resultsColumn
  , tidListColumn
  , testKeyColumn
  , resultColumn
  , passingDateColumn
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Database.Bson.Class
import Model.Types

-- Sex type
data Sex
  = Male
  | Female
  deriving (Show, Read, Eq, Generic)

instance FromJSON Sex
instance ToJSON Sex


instance Bson.Val Sex where
  val = Bson.String . pack . show
  cast' (Bson.String s) = mkSex s

mkSex :: Text -> Maybe Sex
mkSex "Male"    = Just Male
mkSex "Female"  = Just Female
mkSex _         = Nothing


-- TestResult type
data TestResult = TestResult
  { testKey     :: Text
  , result      :: Text
  , passingDate :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON TestResult
instance ToJSON TestResult

instance Bson.Val TestResult where
  val tr = Bson.Doc $ toDocument tr
  cast' (Bson.Doc doc) = fromDocument doc

instance ToDocument TestResult where
  toDocument TestResult{..} =
    [ testKeyColumn     =: testKey
    , resultColumn      =: result
    , passingDateColumn =: passingDate
    ]

instance FromDocument TestResult where
  fromDocument document =
    TestResult
      <$> Bson.lookup testKeyColumn document
      <*> Bson.lookup resultColumn document
      <*> Bson.lookup passingDateColumn document


data User = User
  { uid         :: ID
  , username    :: Text
  , password    :: Text
  , email       :: Text
  , avatar      :: Text
  , firstName   :: Maybe Text
  , secondName  :: Maybe Text
  , birthDay    :: Maybe UTCTime
  , sex         :: Maybe Sex
  , results     :: [TestResult]
  , tidList     :: [ID]
  } deriving (Show, Eq, Generic)

instance ToDocument User where
  toDocument User{..} =
    [ uidColumn        =: uid
    , usernameColumn   =: username
    , passwordColumn   =: password
    , emailColumn      =: email
    , avatarColumn     =: avatar
    , firstNameColumn  =: firstName
    , secondNameColumn =: secondName
    , birthdayColumn   =: birthDay
    , sexColumn        =: sex
    , resultsColumn    =: results
    , tidListColumn    =: tidList
    ]

instance FromDocument User where
  fromDocument document =
    User
      <$> fmap fromObjectId (Bson.lookup uidColumn document)
      <*> Bson.lookup usernameColumn document
      <*> Bson.lookup passwordColumn document
      <*> Bson.lookup emailColumn document
      <*> Bson.lookup avatarColumn document
      <*> Bson.lookup firstNameColumn document
      <*> Bson.lookup secondNameColumn document
      <*> Bson.lookup birthdayColumn document
      <*> Bson.lookup sexColumn document
      <*> Bson.lookup resultsColumn document
      <*> Bson.lookup tidListColumn document



uidColumn           = "_id"         :: Bson.Label
usernameColumn      = "username"    :: Bson.Label
passwordColumn      = "password"    :: Bson.Label
emailColumn         = "email"       :: Bson.Label
avatarColumn        = "avatar"      :: Bson.Label
firstNameColumn     = "firstName"   :: Bson.Label
secondNameColumn    = "secondName"  :: Bson.Label
birthdayColumn      = "birthDay"    :: Bson.Label
sexColumn           = "sex"         :: Bson.Label
resultsColumn       = "results"     :: Bson.Label
tidListColumn       = "tidList"     :: Bson.Label

testKeyColumn       = "testKey"     :: Bson.Label
resultColumn        = "result"      :: Bson.Label
passingDateColumn   = "passingDate" :: Bson.Label
