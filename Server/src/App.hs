module App where

import Network.Wai
import Network.Wai.Handler.Warp
import Model.User
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (lift)
import Config
import Servant
import Servant.Server
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH

import API.API
import Config
import Types

nt :: AppContext -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppContext -> Application
app s = serve apiProxy $ hoistServer apiProxy (nt s) server

server :: ServerT UserAPI AppM
server = getUser :<|> addUser

getUser :: Int -> AppM User
getUser id = undefined


addUser :: User -> AppM Bool
addUser user = undefined