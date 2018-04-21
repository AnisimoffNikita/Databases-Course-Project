module API.Handler where

import Control.Monad.Reader
import Data.Text
import Database.Persist.MongoDB
import Servant
import Servant.Server
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.API
import API.Types
import Model.Model
import Types
import Utils


handler :: ServerT (API auths) AppM
handler = userAPI



userAPI :: AuthResult Login -> ServerT UserAPI AppM
userAPI authResult =
       login
  :<|> newUser
  :<|> getUsername authResult

getUsername :: AuthResult Login -> AppM (Maybe Text)
getUsername (Authenticated user) = return . Just . loginUsername $ user
getUsername _ = return Nothing


login :: Login -> AppM (Headers '[ Header "Set-Cookie" SetCookie
                                 , Header "Set-Cookie" SetCookie
                                 ]
                                 NoContent)
login user = do
  let
    hashed = hashMD5 . loginPassword $ user
    getByUsername = getBy . UniqueUsername . loginUsername $ user
  pool <- asks connectionPool
  mUser <- liftIO $ runMongoDBPoolDef getByUsername pool
  case mUser of
    Nothing -> throwError err401
    Just (Entity _ check) -> do
      if userPassword check == hashed then do
        cookieSettings <- asks cookie
        jwtSettings <- asks jwt
        mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
        case mApplyCookies of
          Nothing           -> throwError err401
          Just applyCookies -> return $ applyCookies NoContent
      else throwError err401


newUser :: UserRegister -> AppM (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ]
                                          NoContent)
newUser reg = do
  let
    user = userRegisterToUser reg
    getByUsername = getBy . UniqueUsername . userUsername $ user
    getByEmail = getBy . UniqueEmail . userEmail $ user
  pool <- asks connectionPool
  mUsername <- liftIO $ runMongoDBPoolDef getByUsername pool
  mEmail <- liftIO $ runMongoDBPoolDef getByEmail pool
  if mUsername /= Nothing || mEmail /= Nothing then throwError err401
  else do
    let
      action = insert user
      logged = userRegisterToLogin reg
    key <- liftIO $ runMongoDBPoolDef action pool
    login logged
