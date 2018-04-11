module Model.TestResult where

import Data.Text (Text)
import Data.Time

data TestResult = TestResult {
    testKey     :: Text
  , result      :: Text
  , passingDate :: UTCTime
} deriving (Show, Eq)
