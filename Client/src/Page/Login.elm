module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

type alias Model = 
    { username : String 
    , password : String
    }


init : (Model, Cmd Msg)
init =
        (Model "" "", Cmd.none)

type Msg
    = Login

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Login ->
            ( model, Cmd.none )


view : Html Msg
view = 
    let 
        form = 
        Form.form []
            [ Form.group []
                [ Form.label [for "myemail"] [ text "Email address"]
                , Input.email [ Input.id "myemail" ]
                , Form.help [] [ text "We'll never share your email with anyone else." ]
                ]
            , Form.group []
                [ Form.label [for "mypwd"] [ text "Password"]
                , Input.password [ Input.id "mypwd" ]
                ]
            , Button.button 
                [ Button.primary
                , Button.attrs [onClick Login]] 
                [ text "Submit" ]
            ]
    in
    Grid.container []
        [ Grid.row
            [ Row.middleXs ]
            [ Grid.col []
                [ form ] 
            ]
        ]