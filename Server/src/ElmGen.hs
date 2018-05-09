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
import           Servant.Multipart
import           Data.List
import           Data.Text               hiding ( intercalate
                                                , map
                                                )
import           Elm

import           API.API
import           API.User.Types
import           API.Types
import           Model.Types                 
import           Model.Model                 

instance ElmType Role
instance ElmType Login
instance ElmType Tokens
instance ElmType JWTData
instance ElmType UserRegister
instance ElmType Profile
instance ElmType Gender
instance ElmType UserInfo
instance ElmType QuestionOptions
instance ElmType QuestionAnswer
instance ElmType Question
instance ElmType Quiz
instance ElmType QuizPreview
instance ElmType QuestionWithoutAnswer
instance ElmType QuizWithoutAnswers

instance HasForeign lang ftype sublayout
    => HasForeign lang ftype (Auth '[JWT] JWTData :> sublayout) where
    type Foreign ftype (Auth '[JWT] JWTData :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

instance HasForeign lang ftype sublayout
    => HasForeign lang ftype (MultipartForm Mem (MultipartData Mem) :> sublayout) where
    type Foreign ftype (MultipartForm Mem (MultipartData Mem) :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

elmGen :: IO ()
elmGen = undefined
  --  do
  -- let code =
  --       "module Api exposing (..)"
  --         : defElmImports
  --         : "type NoContent = NoContent"
  --         : toElmTypeSource (Proxy :: Proxy Login)
  --         : toElmDecoderSource (Proxy :: Proxy Login)
  --         : toElmEncoderSource (Proxy :: Proxy Login)
  --         : toElmTypeSource (Proxy :: Proxy UserRegister)
  --         : toElmDecoderSource (Proxy :: Proxy UserRegister)
  --         : toElmEncoderSource (Proxy :: Proxy UserRegister)
  --         : toElmTypeSource (Proxy :: Proxy Tokens)
  --         : toElmDecoderSource (Proxy :: Proxy Tokens)
  --         : toElmEncoderSource (Proxy :: Proxy Tokens)
  --         : toElmTypeSource (Proxy :: Proxy JWTData)
  --         : toElmDecoderSource (Proxy :: Proxy JWTData)
  --         : toElmEncoderSource (Proxy :: Proxy JWTData)
  --         : toElmTypeSource (Proxy :: Proxy Gender)
  --         : toElmDecoderSource (Proxy :: Proxy Gender)
  --         : toElmEncoderSource (Proxy :: Proxy Gender)
  --         : toElmTypeSource (Proxy :: Proxy Profile)
  --         : toElmDecoderSource (Proxy :: Proxy Profile)
  --         : toElmEncoderSource (Proxy :: Proxy Profile)
  --         : toElmTypeSource (Proxy :: Proxy UserInfo)
  --         : toElmDecoderSource (Proxy :: Proxy UserInfo)
  --         : toElmEncoderSource (Proxy :: Proxy UserInfo)

  --         : toElmTypeSource (Proxy :: Proxy QuestionOptions)
  --         : toElmDecoderSource (Proxy :: Proxy QuestionOptions)
  --         : toElmEncoderSource (Proxy :: Proxy QuestionOptions)
  --         : toElmTypeSource (Proxy :: Proxy QuestionAnswer)
  --         : toElmDecoderSource (Proxy :: Proxy QuestionAnswer)
  --         : toElmEncoderSource (Proxy :: Proxy QuestionAnswer)
  --         : toElmTypeSource (Proxy :: Proxy Question)
  --         : toElmDecoderSource (Proxy :: Proxy Question)
  --         : toElmEncoderSource (Proxy :: Proxy Question)
  --         : toElmTypeSource (Proxy :: Proxy Quiz)
  --         : toElmDecoderSource (Proxy :: Proxy Quiz)
  --         : toElmEncoderSource (Proxy :: Proxy Quiz)
  --         : toElmTypeSource (Proxy :: Proxy QuizPreview)
  --         : toElmDecoderSource (Proxy :: Proxy QuizPreview)
  --         : toElmEncoderSource (Proxy :: Proxy QuizPreview)

  --         : toElmTypeSource (Proxy :: Proxy QuestionWithoutAnswer)
  --         : toElmDecoderSource (Proxy :: Proxy QuestionWithoutAnswer)
  --         : toElmEncoderSource (Proxy :: Proxy QuestionWithoutAnswer)
  --         : toElmTypeSource (Proxy :: Proxy QuizWithoutAnswers)
  --         : toElmDecoderSource (Proxy :: Proxy QuizWithoutAnswers)
  --         : toElmEncoderSource (Proxy :: Proxy QuizWithoutAnswers)
  --         : generateElmForAPI apiProxy
  -- writeFile "../Client/src/Api.elm" $ intercalate "\n\n" $ map unpack code




