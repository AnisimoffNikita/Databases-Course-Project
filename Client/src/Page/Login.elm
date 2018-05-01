module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Router exposing (routeToString)
import Navigation

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
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
    | Register

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Login ->
            ( model, Cmd.none )
        Register ->
            ( model, Navigation.newUrl <| routeToString Router.Register )


view : Html Msg
view = 
    let 
        form = 
        Form.form [onSubmit Login]
            [ Form.group []
                [ Form.label [for "myusername"] [ text "Username"]
                , Input.email [ Input.id "myusername" ]
                ]
            , Form.group []
                [ Form.label [for "mypwd"] [ text "Password"]
                , Input.password [ Input.id "mypwd" ]
                ]
            , Button.button 
                [ Button.primary ]
                [ text "Submit" ]
            ]
    in
    Grid.container 
        [ class "Absolute-Center"
        , class "is-Responsive"
        ]
        [ Grid.row 
            [ Row.middleXs
            , Row.centerXs
            ]
            [ Grid.col 
                [ Col.lg3
                ]
                [ h1 [] [text "Login"]
                , Button.linkButton 
                    [ Button.roleLink
                    , Button.attrs [href <| routeToString Router.Register ]] 
                    [text "No account?"]
                , form ] 
            ]
        ]