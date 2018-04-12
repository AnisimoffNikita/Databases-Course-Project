module Model.TestData where

import Data.Text (Text, pack)
import Data.Time

type TID = Text

data TestData = TestData
  { tid           :: TID
  , name          :: Text
  , description   :: Text
  , creationDate  :: UTCTime
  , passingNumber :: Integer
  , question      :: Text
  } deriving (Show, Eq)

