module Page.Registration exposing (..)

import Html exposing (Html, div, text)
import Data.Session exposing (..)

view : Session -> Html msg
view session = div [] [text "registration"]