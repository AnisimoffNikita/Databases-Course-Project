module Utils exposing (..)

import Html 
import Html.Attributes exposing (href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Router exposing (..)

href_ : msg -> String -> List (Html.Attribute msg)
href_ msg path = 
    [ href path
    , onClickPreventDefault msg
    ]