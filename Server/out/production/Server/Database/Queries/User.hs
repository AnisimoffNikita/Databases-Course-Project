module Database.Queries.User (
    isUserExist
  , newUser
  , updateUsername
  , updatePassword
  , updateEmail
  , updateAvatar
  , updateFirstName
  , updateSecondName
  , updateSex
  , updateBirthDay
  , addTest
  , removeTest
  , addTestResult
  , getUser
  , deleteUser
  , getAllUsers
) where

import Data.Text
import Database.MongoDB((=:))
import qualified Database.MongoDB as Mongo
import Data.Time (UTCTime)

import Model.User
import Model.TestData(TID)
import Database.Bson.User
import Database.Bson.Class

isUserExist :: Text -> Mongo.Action IO Bool
isUserExist username = do
  exist <- Mongo.findOne $ selectColByUID username
  return $ exist == Nothing

newUser :: User -> Mongo.Action IO UID
newUser user = do
  let user' = Mongo.exclude [uidColumn] (toDocument user)
  oid <- Mongo.insert collection user'
  return $ pack.show $ oid


-- TODO:
updateUsername :: UID -> Text -> Mongo.Action IO ()
updateUsername uid username = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ usernameColumn =: username]]

updatePassword :: UID -> Text -> Mongo.Action IO ()
updatePassword uid password = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ passwordColumn =: password]]

updateEmail :: UID -> Text -> Mongo.Action IO ()
updateEmail uid email = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ emailColumn =: email]]

updateAvatar :: UID -> Text -> Mongo.Action IO ()
updateAvatar uid avatar = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ avatarColumn =: avatar]]

updateFirstName :: UID -> Maybe Text -> Mongo.Action IO ()
updateFirstName uid firstName = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ firstNameColumn =: firstName]]

updateSecondName :: UID -> Maybe Text -> Mongo.Action IO ()
updateSecondName uid secondName = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ secondNameColumn =: secondName]]

updateSex :: UID -> Maybe Sex -> Mongo.Action IO ()
updateSex uid sex = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ sexColumn =: sex]]

updateBirthDay :: UID -> Maybe UTCTime -> Mongo.Action IO ()
updateBirthDay uid birthday = Mongo.modify
                          (selectColByUID uid)
                          ["$set" =: [ birthdayColumn =: birthday]]

addTest :: UID -> TID -> Mongo.Action IO ()
addTest uid tid = Mongo.modify
                  (selectColByUID uid)
                  ["$push" =: [ tidListColumn =: tid]]

removeTest :: UID -> TID -> Mongo.Action IO ()
removeTest uid tid = Mongo.modify
                  (selectColByUID uid)
                  ["$pull" =: [ tidListColumn =: tid]]

addTestResult :: UID -> TestResult -> Mongo.Action IO ()
addTestResult uid testResult = Mongo.modify
                  (selectColByUID uid)
                  ["$push" =: [ resultsColumn =: testResult]]

getUser :: UID -> Mongo.Action IO (Maybe Mongo.Document)
getUser uid = Mongo.findOne (selectColByUID uid)

deleteUser :: UID -> Mongo.Action IO ()
deleteUser uid = Mongo.deleteOne (selectColByUID uid)

getAllUsers :: Mongo.Action IO [Mongo.Document]
getAllUsers = Mongo.rest =<< Mongo.find selectAllCol

collection :: Mongo.Collection
collection = "users"

selectAllCol :: Mongo.Select a => a
selectAllCol = Mongo.select [] collection

selectColByUID :: Mongo.Select a => Text -> a
selectColByUID uid = Mongo.select [uidColumn =: oid] collection
  where
    oid = read (unpack uid) :: Mongo.ObjectId

