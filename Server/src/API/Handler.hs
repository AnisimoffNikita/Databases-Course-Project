module API.Handler where

import Servant
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Utils.StaticFiles 

import API.API
import API.User.Handler
import API.Quiz.Handler
import Types


handler :: ServerT (API auths) AppM
handler auths = userAPI auths :<|> quizAPI auths


staticHandler :: Server Raw 
staticHandler = serveDirectoryWebApp "static"