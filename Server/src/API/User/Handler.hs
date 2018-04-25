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
import           Model.Model
import           Types
import           Utils

userAPI :: AuthResult JWTData -> ServerT UserAPI AppM
userAPI authResult = login :<|> newUser :<|> getUsername authResult

getUsername :: AuthResult JWTData -> AppM (Maybe Text)
getUsername (Authenticated user) = return . Just . jwtUsername $ user
getUsername _                    = return Nothing


login :: Login -> AppM Tokens
login user = do
  exists <- userExists user
  if exists
    then do
      jwtSettings <- asks jwt
      let jwt = JWTData . loginUsername $ user
      etoken <- liftIO $ makeJWT jwt jwtSettings Nothing
      case etoken of
        Left  _ -> throwError err401
        Right v -> return $ Tokens (decodeUtf8 . toStrict $ v)
    else throwError err401

userExists :: Login -> AppM Bool
userExists user = do
  let hashed        = hashMD5 . loginPassword $ user
      getByUsername = getBy . UniqueUsername . loginUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> return False
    Just (Entity _ check) -> return $ userPassword check == hashed


newUser :: UserRegister -> AppM Tokens
newUser reg = do
  let user          = userRegisterToUser reg
      getByUsername = getBy . UniqueUsername . userUsername $ user
      getByEmail    = getBy . UniqueEmail . userEmail $ user
  pool      <- asks connectionPool
  mUsername <- liftIO $ runMongoDBPoolDef getByUsername pool
  mEmail    <- liftIO $ runMongoDBPoolDef getByEmail pool
  if isJust mUsername || isJust mEmail
    then throwError err401
    else do
      let action = insert user
          logged = userRegisterToLogin reg
      void $ liftIO $ runMongoDBPoolDef action pool
      login logged
