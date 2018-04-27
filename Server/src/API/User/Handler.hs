module API.User.Handler
where

import           Control.Monad.Reader
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Maybe (isJust)
import           Data.Text (Text)
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
  :<|> getUserList authResult

getUsername :: AuthResult JWTData -> AppM (ResponseResult Text)
getUsername (Authenticated user) = return $ responseOk . jwtUsername $ user
getUsername _                    = return $ responseError 401 "not athorized"


login :: AuthResult JWTData -> Login -> AppM (ResponseResult Tokens)
login (Authenticated user) _ = return $ responseError 400 "already authorized"
login _ user = do
  exists <- userExists user
  if exists
    then do
      jwtSettings <- asks jwt
      let jwtData = JWTData . loginUsername $ user
      etoken <- liftIO $ makeJWT jwtData jwtSettings Nothing
      case etoken of
        Left  _ -> return $ responseError 500 "something went worng"
        Right v -> return $ responseOk $ Tokens (decodeUtf8 . toStrict $ v)
    else return $ responseError 401 "no such user"

userExists :: Login -> AppM Bool
userExists user = do
  let hashed        = hashMD5 . loginPassword $ user
      getByUsername = getBy . UniqueUsername . loginUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing               -> return False
    Just (Entity _ check) -> return $ userPassword check == hashed

newUser :: AuthResult JWTData -> UserRegister -> AppM (ResponseResult Tokens)
newUser (Authenticated user) _ =
  return $ responseError 400 "already authorized"
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


getUserInfo :: AuthResult JWTData -> AppM (ResponseResult Profile)
getUserInfo (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing              -> return $ responseError 401 "no such user"
    Just (Entity _ user) -> return $ responseOk $ userToProfile user
getUserInfo _ = return $ responseError 400 "incorrect data"

getUserList :: AuthResult JWTData -> AppM (ResponseResult [Profile])
getUserList (Authenticated user) = do 
  let selectAll = selectList [] []
  pool  <- asks connectionPool
  users :: [Entity User] <- liftIO $ runMongoDBPoolDef selectAll pool
  return $ responseOk $ map (\(Entity _ user) -> userToProfile user) users
getUserList _ = return $ responseError 400 "incorrect data"
