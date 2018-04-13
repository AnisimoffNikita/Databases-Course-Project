module Utils.Types where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Text
import Database.Persist.MongoDB


instance Aeson.ToJSON ObjectId where
  toJSON = Aeson.toJSON . pack . show

instance Aeson.FromJSON ObjectId where
  parseJSON (Aeson.String v) = return . read . unpack $ v
  parseJSON _ = mzero