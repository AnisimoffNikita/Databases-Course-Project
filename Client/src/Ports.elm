port module Ports exposing (..)

import Data.Session exposing (Session)

port setStorage : Session -> Cmd msg


setStorageHelper : Session -> ( Session, Cmd msg )
setStorageHelper model =
    ( model, setStorage model )