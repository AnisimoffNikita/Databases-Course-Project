module Msgs exposing (..)

import Http
import Navigation exposing (Location)
import RemoteData exposing (WebData)


type Msg
    = OnLocationChange Location