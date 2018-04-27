module API.User.Handler
where

import           Control.Monad.Reader
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Maybe (isJust)
import           Data.Text
import           Data.Text.Encoding             ( decodeUtf8 )
import           Database.Persist.MongoDB
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan  ( )

import           API.User.API
import           API.User.Types
import           API.Types 
import           Model.Model
import           Types
import           Utils

userAPI :: AuthResult JWTData -> ServerT UserAPI AppM
userAPI authResult = 
       login authResult 
  :<|> newUser authResult 
  :<|> getUsername authResult 
  :<|> getUserInfo authResult

getUsername :: AuthResult JWTData -> AppM (ResponseResult Text)
getUsername (Authenticated user) = return $ responseOk . jwtUsername $ user
getUsername _                    = return $ responseError 401 "not athorized"


login :: AuthResult JWTData -> Login -> AppM (ResponseResult Tokens)
login _ user = do
  exists <- userExists user
  if exists
    then do
      jwtSettings <- asks jwt
      let jwt = JWTData . loginUsername $ user
      etoken <- liftIO $ makeJWT jwt jwtSettings Nothing
      case etoken of
        Left  _ -> return $ responseError 500 "something went worng"
        Right v -> return $ responseOk $ Tokens (decodeUtf8 . toStrict $ v)
    else return $ responseError 401 "no such user"

newUser :: AuthResult JWTData -> UserRegister -> AppM (ResponseResult Tokens)
newUser (Authenticated user) _ = throwError err401 
newUser auth reg = do
  let user          = userRegisterToUser reg
      getByUsername = getBy . UniqueUsername . userUsername $ user
      getByEmail    = getBy . UniqueEmail . userEmail $ user
  pool      <- asks connectionPool
  mUsername <- liftIO $ runMongoDBPoolDef getByUsername pool
  mEmail    <- liftIO $ runMongoDBPoolDef getByEmail pool
  if isJust mUsername || isJust mEmail
    then return $ responseError 500 "something went worng"
    else do
      let action = insert user
          logged = userRegisterToLogin reg
      void $ liftIO $ runMongoDBPoolDef action pool
      login auth logged


getUserInfo :: AuthResult JWTData -> AppM (ResponseResult UserInfo)
getUserInfo (Authenticated user) = do 
  let 
    getByUsername = getBy . UniqueUsername . userUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of 
    Nothing -> return $ responseError 401 "no such user"
    Just user -> return $ responseOk $ userToUserInfo user