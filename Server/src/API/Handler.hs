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
handler = check :<|> protected :<|> unprotected


check :: ServerT (Check auths) AppM
check = checkLogin

checkLogin :: AuthResult User -> AppM (Maybe Text)
checkLogin (Authenticated user) = loggedIn
checkLogin _ = notLoggedIn

loggedIn :: AppM (Maybe Text)
loggedIn = undefined

notLoggedIn :: AppM (Maybe Text)
notLoggedIn = undefined



protected :: AuthResult User -> ServerT Protected AppM
protected (Authenticated user) =
       getUser

getUser :: Text -> AppM (Maybe User)
getUser oid = undefined

unprotected :: ServerT Unprotected AppM
unprotected =
       login
  :<|> newUser
  :<|> confirmUser

login :: Login -> AppM (Headers '[ Header "Set-Cookie" SetCookie
                                    , Header "Set-Cookie" SetCookie]
                                   NoContent)
login user = undefined

newUser :: User -> AppM NoContent
newUser user = undefined

confirmUser :: Text -> AppM NoContent
confirmUser jwt = undefined