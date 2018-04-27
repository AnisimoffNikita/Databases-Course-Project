module API.Utils
where

import           Data.Char                      ( toLower )
import           Data.Aeson

optionsWithoutPrefix prefLen =
    defaultOptions { fieldLabelModifier = map toLower . drop prefLen }

