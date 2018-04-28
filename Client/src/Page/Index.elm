module Page.Index exposing (..)

import Html exposing (Html, div, text)
import Data.Session exposing (..)

index : Session -> Html msg -> Html msg
index session main = div [] [text "header", main, text "footer"]