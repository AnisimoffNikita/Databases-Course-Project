module Utils where

import Data.ByteString (ByteString)
import Data.Text

import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash.MD5 (hash)

avatarDefault :: Text
avatarDefault = "no_avatar.png"


hashMD5 :: Text -> ByteString
hashMD5 = hash.encodeUtf8