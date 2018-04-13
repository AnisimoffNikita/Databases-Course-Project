module Database.Queries.User (
    isUserExist
  , newUser
  , getUser
  , updateUsername
  , updatePassword
  , updateEmail
  , updateAvatar
  , updateFirstName
  , updateSecondName
  , updateSex
  , updateBirthDay
  , addUserTest
  , deleteUserTest
  , addTestResult
  , getUserTests
  , deleteUser
  , getAllUsers
) where

import Data.Text (Text, pack)
import Database.MongoDB((=:))
import qualified Database.MongoDB as Mongo
import Data.Time (UTCTime)

import Model.User
import Model.Types
import Database.Bson.Class

isUserExist :: Text -> Mongo.Action IO Bool
isUserExist username = do
  exist <- Mongo.findOne $ selectColByUID username
  return $ exist == Nothing

newUser :: User -> Mongo.Action IO ID
newUser user = do
  let user' = Mongo.exclude [uidColumn] (toDocument user)
  oid <- Mongo.insert collection user'
  return $ pack.show $ oid

getUser :: ID -> Mongo.Action IO (Maybe User)
getUser uid = do
  jx <- Mongo.findOne (selectColByUID uid)
  return $ jx >>= fromDocument

updateUsername :: ID -> Text -> Mongo.Action IO ()
updateUsername uid username =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ usernameColumn =: username]]

updatePassword :: ID -> Text -> Mongo.Action IO ()
updatePassword uid password =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ passwordColumn =: password]]

updateEmail :: ID -> Text -> Mongo.Action IO ()
updateEmail uid email =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ emailColumn =: email]]

updateAvatar :: ID -> Text -> Mongo.Action IO ()
updateAvatar uid avatar =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ avatarColumn =: avatar]]

updateFirstName :: ID -> Maybe Text -> Mongo.Action IO ()
updateFirstName uid firstName =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ firstNameColumn =: firstName]]

updateSecondName :: ID -> Maybe Text -> Mongo.Action IO ()
updateSecondName uid secondName =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ secondNameColumn =: secondName]]

updateSex :: ID -> Maybe Sex -> Mongo.Action IO ()
updateSex uid sex =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ sexColumn =: sex]]

updateBirthDay :: ID -> Maybe UTCTime -> Mongo.Action IO ()
updateBirthDay uid birthday =
  Mongo.modify
    (selectColByUID uid)
    ["$set" =: [ birthdayColumn =: birthday]]

addUserTest :: ID -> ID -> Mongo.Action IO ()
addUserTest uid tid =
  Mongo.modify
    (selectColByUID uid)
    ["$push" =: [ tidListColumn =: tid]]

addTestResult :: ID -> TestResult -> Mongo.Action IO ()
addTestResult uid testResult =
  Mongo.modify
    (selectColByUID uid)
    ["$push" =: [ resultsColumn =: testResult]]

deleteUserTest :: ID -> ID -> Mongo.Action IO ()
deleteUserTest uid tid =
  Mongo.modify
    (selectColByUID uid)
    ["$pull" =: [ tidListColumn =: tid]]

getUserTests :: ID -> Mongo.Action IO (Maybe [ID])
getUserTests uid = do
  x <- getUser uid
  return $ tidList <$> x

deleteUser :: ID -> Mongo.Action IO ()
deleteUser uid =
  Mongo.deleteOne (selectColByUID uid)

getAllUsers :: Mongo.Action IO [Mongo.Document]
getAllUsers = Mongo.find selectAllCol >>= Mongo.rest

collection :: Mongo.Collection
collection = "users"

selectAllCol :: Mongo.Select a => a
selectAllCol = Mongo.select [] collection

selectColByUID :: Mongo.Select a => Text -> a
selectColByUID uid = Mongo.select [uidColumn =: toObjectId uid] collection

