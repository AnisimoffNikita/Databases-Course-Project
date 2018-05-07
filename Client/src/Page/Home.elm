module Page.Home exposing (..)

import Data.Session exposing (..)
import Html exposing (Html, div, text)


view : Session -> Html msg
view session =
    div [] [ text "Home" ]
