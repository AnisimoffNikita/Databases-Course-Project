module Utils where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Base16
import Data.Text
import Servant.Server

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Crypto.Hash.MD5 (hash)

avatarDefault = "no_avatar.png" :: Text


hashMD5 = hash.encodeUtf8