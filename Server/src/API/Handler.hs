module API.Handler where

import Data.Text
import Servant
import Servant.Server
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import API.API
import API.Types
import Model.User
import Types


handler :: ServerT (API auths) AppM
handler = protected :<|> unprotected


protected :: AuthResult User -> ServerT Protected AppM
protected authResult =
       getLoggedIn authResult
  :<|> getUser authResult

getLoggedIn :: AuthResult User -> AppM (Maybe Text)
getLoggedIn (Authenticated user) = loggedIn
getLoggedIn _ = notLoggedIn

loggedIn :: AppM (Maybe Text)
loggedIn = undefined

notLoggedIn :: AppM (Maybe Text)
notLoggedIn = undefined

getUser :: AuthResult User -> Text -> AppM (Maybe User)
getUser oid = undefined

unprotected :: ServerT Unprotected AppM
unprotected =
       login
  :<|> newUser
  :<|> confirmUser

login :: Login -> AppM (Headers '[ Header "Set-Cookie" SetCookie
                                 , Header "Set-Cookie" SetCookie
                                 ]
                                 NoContent)
login user = undefined

newUser :: User -> AppM NoContent
newUser user = undefined

confirmUser :: Text -> AppM NoContent
confirmUser jwt = undefined