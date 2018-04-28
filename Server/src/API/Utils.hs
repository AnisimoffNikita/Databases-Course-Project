module API.Utils
where

import Control.Monad.Error.Class
import           Data.Char                      ( toLower )
import           Data.Aeson
import           Data.Text (Text)
import GHC.Generics
import Servant
import Servant.Server

optionsWithoutPrefix :: Int -> Options
optionsWithoutPrefix prefLen =
    defaultOptions { fieldLabelModifier = map toLower . drop prefLen }


data ErrorBody = ErrorBody 
  { message :: Text 
  , code :: Int 
  } deriving (Show, Read, Eq, Generic)

instance ToJSON ErrorBody
instance FromJSON ErrorBody

badStatus :: ServantErr -> Text -> Int -> ServantErr
badStatus status message code = status {errBody = body}
  where
    body = encode $ ErrorBody message code