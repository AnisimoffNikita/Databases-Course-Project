port module Ports exposing (..)

import Data.Session exposing (Session)


port setStorage : Session -> Cmd msg
