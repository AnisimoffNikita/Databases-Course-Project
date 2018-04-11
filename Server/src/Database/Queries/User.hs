module Database.Queries.User where

import Data.Text
import qualified Database.MongoDB as Mongo

import Model.User
import Database.Bson.User

insert :: User -> Mongo.Action IO ()
insert user = Mongo.insert



