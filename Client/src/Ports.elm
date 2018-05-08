port module Ports exposing (..)

import Data.Session exposing (Session)


port setStorage : Session -> Cmd msg


port fileSelected : String -> Cmd msg

type alias ImagePortData =
    { contents : String
    , filename : String
    }

port fileContentRead : (ImagePortData -> msg) -> Sub msg