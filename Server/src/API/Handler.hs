module API.Handler where

import Servant
import Servant.Auth.Server.SetCookieOrphan ()

import API.API
import API.User.Handler
import Types

handler :: ServerT (API auths) AppM
handler = userAPI



