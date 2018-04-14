module API.API where

import Servant

import Model.User

type UserAPI = "user" :> Capture "id" Int :> Get '[JSON] User
          :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] Bool

apiProxy :: Proxy UserAPI
apiProxy = Proxy