module Utils where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Maybe (runMaybeT)
import Servant.Server
import Data.Pool (withResource)
