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

instance ElmType Role
instance ElmType a => ElmType (ResponseResult a)
instance ElmType Login
instance ElmType Tokens
instance ElmType JWTData
instance ElmType UserRegister
instance ElmType Profile

instance HasForeign lang ftype sublayout
    => HasForeign lang ftype (Auth '[JWT] JWTData :> sublayout) where
    type Foreign ftype (Auth '[JWT] JWTData :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

genElm :: IO ()
genElm = do
  let code = "module Api exposing (..)" :
            defElmImports :
            "type NoContent = NoContent" :
            toElmTypeSource (Proxy :: Proxy Login) :
            toElmDecoderSource (Proxy :: Proxy Login) :
            toElmEncoderSource (Proxy :: Proxy Login) :

            toElmTypeSource (Proxy :: Proxy Tokens) :
            toElmDecoderSource (Proxy :: Proxy Tokens) :
            toElmEncoderSource (Proxy :: Proxy Tokens) :
            
            toElmTypeSource (Proxy :: Proxy JWTData) :
            toElmDecoderSource (Proxy :: Proxy JWTData) :
            toElmEncoderSource (Proxy :: Proxy JWTData) :
            
            toElmTypeSource (Proxy :: Proxy (ResponseResult Tokens)) :
            toElmDecoderSource (Proxy :: Proxy (ResponseResult Tokens)) :
            toElmEncoderSource (Proxy :: Proxy (ResponseResult Tokens)) :
            
            toElmTypeSource (Proxy :: Proxy (ResponseResult Profile)) :
            toElmDecoderSource (Proxy :: Proxy (ResponseResult Profile)) :
            toElmEncoderSource (Proxy :: Proxy (ResponseResult Profile)) :

            toElmTypeSource (Proxy :: Proxy (ResponseResult [Profile])) :
            toElmDecoderSource (Proxy :: Proxy (ResponseResult [Profile])) :
            toElmEncoderSource (Proxy :: Proxy (ResponseResult [Profile])) :
            generateElmForAPI apiProxy
  writeFile "../Client/src/Api.elm" $ intercalate "\n\n" $ map unpack code




