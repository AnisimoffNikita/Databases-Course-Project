module Database.Bson.User (

) where

import Data.Text (Text, pack)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Database.Bson.Class
import Model.User
import Database.Bson.TestResult

instance Bson.Val Sex where
  val = Bson.String . pack . show
  cast' (Bson.String s) = mkSex s

mkSex :: Text -> Maybe Sex
mkSex "Male"    = Just Male
mkSex "Female"  = Just Female
mkSex _         = Nothing

instance ToDocument User where
  toDocument User{..} =
    [ "username"   =: username
    , "password"   =: password
    , "email"      =: email
    , "avatar"     =: avatar
    , "results"    =: results
    , "testKeys"   =: testKeys
    , "firstName"  =: firstName
    , "secondName" =: secondName
    , "birthDay"   =: birthDay
    , "sex"        =: sex
    ]

instance FromDocument User where
  fromDocument document =
    User
      <$> Bson.lookup "username" document
      <*> Bson.lookup "password" document
      <*> Bson.lookup "email" document
      <*> Bson.lookup "avatar" document
      <*> Bson.lookup "results" document
      <*> Bson.lookup "testKeys" document
      <*> Bson.lookup "firstName" document
      <*> Bson.lookup "secondName" document
      <*> Bson.lookup "birthDay" document
      <*> Bson.lookup "sex" document
