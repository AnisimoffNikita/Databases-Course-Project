module API.Handler where

import Servant
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Utils.StaticFiles 

import API.API
import API.User.Handler
import Types

handler :: ServerT (API auths) AppM
handler = userAPI 


staticHandler :: Server Raw 
staticHandler = serveDirectoryWebApp "static"