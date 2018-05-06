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
import           API.Utils
import           Model.Model
import           Types
import           Utils
import           Debug.Trace


userAPI :: AuthResult JWTData -> ServerT UserAPI AppM
userAPI authResult =
       login authResult
  :<|> newUser authResult
  :<|> getUsername authResult
  :<|> getProfile authResult
  :<|> editUsername authResult
  :<|> editPassword authResult
  :<|> editEmail authResult
  :<|> editAvatar authResult
  :<|> editUserInfo authResult


getUsername :: AuthResult JWTData -> AppM Text
getUsername (Authenticated user) = return $ jwtUsername  user
getUsername _                    = throwError $ badStatus err400 "" 1


login :: AuthResult JWTData -> Login -> AppM Tokens
login (Authenticated user) _ = throwError $ badStatus err400 "" 1
login _ user = do
  exists <- userExists user
  if exists
    then do
      jwtSettings <- asks jwt
      let jwtData = JWTData . loginUsername $ user
      etoken <- liftIO $ makeJWT jwtData jwtSettings Nothing
      case etoken of
        Left  _ -> throwError $ badStatus err500 "" 1
        Right v -> return $ Tokens (decodeUtf8 . toStrict $ v)
    else throwError $ badStatus err400 "" 1

userExists :: Login -> AppM Bool
userExists user = do
  let hashed        = hashMD5 . loginPassword $ user
      getByUsername = getBy . UniqueUsername . loginUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  trace (show user) $ case mUser of
    Nothing               -> return False
    Just (Entity _ check) -> return $ userPassword check == hashed

newUser :: AuthResult JWTData -> UserRegister -> AppM Tokens
newUser (Authenticated user) _ =
  throwError $ badStatus err400 "" 1
newUser auth reg = do
  let user          = userRegisterToUser reg
      getByUsername = getBy . UniqueUsername . userUsername $ user
      getByEmail    = getBy . UniqueEmail . userEmail $ user
  pool      <- asks connectionPool
  mUsername <- liftIO $ runMongoDBPoolDef getByUsername pool
  mEmail    <- liftIO $ runMongoDBPoolDef getByEmail pool
  if isJust mUsername || isJust mEmail
    then throwError $ badStatus err500 "" 1
    else do
      let action = insert user
          logged = userRegisterToLogin reg
      void $ liftIO $ runMongoDBPoolDef action pool
      login auth logged


getProfile :: AuthResult JWTData -> AppM Profile
getProfile (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing              -> throwError $ err401 {errBody = "error"} 
    Just (Entity _ user) -> return $ userToProfile user
getProfile _ = throwError $ err400 {errBody = "error"} 

getUserList :: AuthResult JWTData -> AppM [Profile]
getUserList (Authenticated user) = do 
  let selectAll = selectList [] []
  pool  <- asks connectionPool
  users :: [Entity User] <- liftIO $ runMongoDBPoolDef selectAll pool
  return $  map (\(Entity _ user) -> userToProfile user) users
getUserList _ = throwError $ err400 {errBody = "error"} 


usernameExists :: Text -> AppM Bool
usernameExists username = do
  let getByUsername = getBy . UniqueUsername $ username
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing -> return False
    Just _  -> return True

emailExists :: Text -> AppM Bool
emailExists email = do
  let getByUsername = getBy . UniqueEmail $ email
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing -> return False
    Just _  -> return True

editUsername :: AuthResult JWTData -> Text -> AppM NoContent
editUsername (Authenticated user) username = do
  exists <- usernameExists username
  if exists then throwError $ err400 {errBody = "Such username already exist"} 
  else do
    let getByUsername = getBy . UniqueUsername . jwtUsername $ user
    pool  <- asks connectionPool
    mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
    case mUser of
      Nothing              -> throwError $ err401 {errBody = "error"} 
      Just (Entity id user) -> do
        let edit = update id [UserUsername =. username]
        liftIO $ runMongoDBPoolDef edit pool
        return NoContent
editUsername _ _ = throwError $ err400 {errBody = "error"} 

editPassword :: AuthResult JWTData -> Text -> AppM NoContent
editPassword (Authenticated user) password = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
      hashed        = hashMD5 password
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing              -> throwError $ err401 {errBody = "error"} 
    Just (Entity id user) -> do
      let edit = update id [UserPassword =. hashed]
      liftIO $ runMongoDBPoolDef edit pool
      return NoContent
editPassword _ _ = throwError $ err400 {errBody = "error"} 


editEmail :: AuthResult JWTData -> Text -> AppM NoContent
editEmail (Authenticated user) email = do
  exists <- emailExists email
  if exists then throwError $ err400 {errBody = "Such email is already in use"} 
  else do
    let getByUsername = getBy . UniqueUsername . jwtUsername $ user
    pool  <- asks connectionPool
    mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
    case mUser of
      Nothing              -> throwError $ err401 {errBody = "error"} 
      Just (Entity id user) -> do
        let edit = update id [UserEmail =. email]
        liftIO $ runMongoDBPoolDef edit pool
        return NoContent
editEmail _ _ = throwError $ err400 {errBody = "error"} 

editAvatar :: AuthResult JWTData -> AppM Text
editAvatar (Authenticated user) = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing              -> throwError $ err401 {errBody = "error"} 
    Just (Entity id user) -> error "no editAvatar implementation"
editAvatar _ = throwError $ err400 {errBody = "error"} 



editUserInfo :: AuthResult JWTData -> UserInfo -> AppM NoContent
editUserInfo (Authenticated user) UserInfo{..} = do
  let getByUsername = getBy . UniqueUsername . jwtUsername $ user
  pool  <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing              -> throwError $ err401 {errBody = "error"} 
    Just (Entity id user) -> do
      let edit = update id [ UserBirthday =. infoBirthday
                           , UserFirstName =. infoFirstName
                           , UserSecondName =. infoSecondName
                           , UserGender =. infoGender]
      liftIO $ runMongoDBPoolDef edit pool
      return NoContent
editUserInfo _ _ = throwError $ err400 {errBody = "error"} 
