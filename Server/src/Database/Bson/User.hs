module Database.Bson.User (
    uidColumn
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

import Data.Text (Text, pack)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Database.Bson.Class
import Model.User

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
      <$> fmap (pack . show) (Bson.lookup uidColumn document :: Maybe Bson.ObjectId)
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


instance Bson.Val TestResult where
  val tr = Bson.Doc $ toDocument tr
  cast' (Bson.Doc doc) = fromDocument doc


instance Bson.Val Sex where
  val = Bson.String . pack . show
  cast' (Bson.String s) = mkSex s

mkSex :: Text -> Maybe Sex
mkSex "Male"    = Just Male
mkSex "Female"  = Just Female
mkSex _         = Nothing


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
