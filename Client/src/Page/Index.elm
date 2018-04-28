module Page.Index exposing (..)

import Html exposing (Html, Attribute, div, text, a, p)
import Data.Session exposing (..)
import Router exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode
import Msgs

index : Session -> Html Msgs.Msg -> Html Msgs.Msg
index session main = 
    let 
        navbar = 
            div []
                [ p [] 
                    [a  [href Home
                        , onClickPreventDefault (Msgs.NavigateTo <| routeToString Home)] 
                        [text "home"]]
                , p [] 
                    [a  [ href Dashboard
                        , onClickPreventDefault (Msgs.NavigateTo <| routeToString Dashboard)] 
                        [text "dashboard"]]
                , p [] 
                    [a  [href Login
                        , onClickPreventDefault (Msgs.NavigateTo <| routeToString Login)] 
                        [text "login"]]
                , p [] 
                    [a  [href Registration
                        , onClickPreventDefault (Msgs.NavigateTo <| routeToString Registration)]  
                        [text "registration"]]
                ]
    in
    div [] 
        [navbar, main, text "footer"]


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
  onWithOptions
    "click"
    { preventDefault = True
    , stopPropagation = False
    }
    (Json.Decode.succeed msg)