module Page.Register exposing (..)

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
    , email : String
    , password : String
    }


init : (Model, Cmd Msg)
init =
        (Model "" "" "", Cmd.none)

type Msg
    = Register
    | Login

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Register ->
            ( model, Cmd.none )
        Login ->
            ( model, Navigation.newUrl <| routeToString Router.Login )


view : Html Msg
view = 
    let 
        form = 
        Form.form [onSubmit Register]
            [ Form.group []
                [ Form.label [for "myemail"] [ text "Email address"]
                , Input.email [ Input.id "myemail" ]
                ]
            , Form.group []
                [ Form.label [for "myusername"] [ text "Username"]
                , Input.email [ Input.id "myusername" ]
                ]
            , Form.group []
                [ Form.label [for "mypwd"] [ text "Password"]
                , Input.password [ Input.id "mypwd" ]
                ]
            , Button.button 
                [ Button.primary
                ] 
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
                [ Col.md3
                ]
                [ h1 [] [text "Register"]
                , Button.linkButton 
                    [ Button.roleLink
                    , Button.attrs [href <| routeToString Router.Login ]] 
                    [text "Have account?"]
                , form ] 
            ]
        ]