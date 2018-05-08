{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ElmGen
where

import           Data.Proxy
import           GHC.TypeLits                   ( KnownSymbol )
import           Servant.Foreign
import           Servant.Auth.Server
import           Servant.Elm
import           Data.List
import           Data.Text               hiding ( intercalate
                                                , map
                                                )
import           Elm

import           API.API
import           API.User.Types
import           API.Types
import           Model.Types                    ( Gender )

instance ElmType Role
instance ElmType Login
instance ElmType Tokens
instance ElmType JWTData
instance ElmType UserRegister
instance ElmType Profile
instance ElmType Gender
instance ElmType UserInfo

instance HasForeign lang ftype sublayout
    => HasForeign lang ftype (Auth '[JWT] JWTData :> sublayout) where
    type Foreign ftype (Auth '[JWT] JWTData :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

-- elmGen :: IO ()
-- elmGen = do
--   let code =
--         "module Api exposing (..)"
--           : defElmImports
--           : "type NoContent = NoContent"
--           : toElmTypeSource (Proxy :: Proxy Login)
--           : toElmDecoderSource (Proxy :: Proxy Login)
--           : toElmEncoderSource (Proxy :: Proxy Login)
--           : toElmTypeSource (Proxy :: Proxy UserRegister)
--           : toElmDecoderSource (Proxy :: Proxy UserRegister)
--           : toElmEncoderSource (Proxy :: Proxy UserRegister)
--           : toElmTypeSource (Proxy :: Proxy Tokens)
--           : toElmDecoderSource (Proxy :: Proxy Tokens)
--           : toElmEncoderSource (Proxy :: Proxy Tokens)
--           : toElmTypeSource (Proxy :: Proxy JWTData)
--           : toElmDecoderSource (Proxy :: Proxy JWTData)
--           : toElmEncoderSource (Proxy :: Proxy JWTData)
--           : toElmTypeSource (Proxy :: Proxy Gender)
--           : toElmDecoderSource (Proxy :: Proxy Gender)
--           : toElmEncoderSource (Proxy :: Proxy Gender)
--           : toElmTypeSource (Proxy :: Proxy Profile)
--           : toElmDecoderSource (Proxy :: Proxy Profile)
--           : toElmEncoderSource (Proxy :: Proxy Profile)
--           : toElmTypeSource (Proxy :: Proxy UserInfo)
--           : toElmDecoderSource (Proxy :: Proxy UserInfo)
--           : toElmEncoderSource (Proxy :: Proxy UserInfo)
--           : generateElmForAPI apiProxy
--   writeFile "../Client/src/Api.elm" $ intercalate "\n\n" $ map unpack code




