module Page.Register exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Data.Tokens as Tokens exposing (..)
import Html exposing (Html, h1, text)
import Html.Attributes exposing (class, for, href)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value, object)
import Navigation
import RemoteData exposing (WebData)
import Router exposing (routeToString)


type alias UserRegister =
    { username : String
    , email : String
    , password : String
    }


encodeUserRegister : UserRegister -> Value
encodeUserRegister x =
    object
        [ ( "username", Encode.string x.username )
        , ( "email", Encode.string x.email )
        , ( "password", Encode.string x.password )
        ]


type alias Model =
    { username : String
    , email : String
    , password : String
    , tokens : WebData Tokens
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "" RemoteData.NotAsked, Cmd.none )


type Msg
    = Register
    | Login
    | TokensRecieved UserRegister (WebData Tokens)
    | Username String
    | Email String
    | Password String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Register ->
            ( model, createPostCommand model )

        Login ->
            ( model, Navigation.newUrl <| routeToString Router.Login )

        TokensRecieved user response ->
            ( { model | tokens = response }, Cmd.none )

        Username username ->
            ( { model | username = username }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )


createPostCommand : Model -> Cmd Msg
createPostCommand model =
    let
        body =
            { username = model.username
            , email = model.email
            , password = model.password
            }
    in
    postUserNew body
        |> RemoteData.sendRequest
        |> Cmd.map (TokensRecieved body)


postUserNew : UserRegister -> Http.Request Tokens
postUserNew body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url = "http://localhost:8080/user/register"
        , body =
            Http.jsonBody (encodeUserRegister body)
        , expect =
            Http.expectJson decodeTokens
        , timeout =
            Nothing
        , withCredentials =
            False
        }


view : Html Msg
view =
    let
        form =
            Form.form [ onSubmit Register ]
                [ Form.group []
                    [ Form.label [ for "myemail" ] [ text "Email" ]
                    , Input.email [ Input.id "myemail", Input.attrs [ onInput Email ], Input.value ""  ]
                    ]
                , Form.group []
                    [ Form.label [ for "myusername" ] [ text "Логин" ]
                    , Input.text [ Input.id "myusername",  Input.onInput Username, Input.value "" ]
                    ]
                , Form.group []
                    [ Form.label [ for "mypwd" ] [ text "Пароль" ]
                    , Input.password [ Input.id "mypwd", Input.attrs [ onInput Password ], Input.value ""  ]
                    ]
                , Button.button
                    [ Button.primary
                    ]
                    [ text "Создать аккаунт" ]
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
                [ h1 [] [ text "Регистрация" ]
                , Button.linkButton
                    [ Button.roleLink
                    , Button.attrs [ href <| routeToString Router.Login ]
                    ]
                    [ text "Уже есть аккаунт?" ]
                , form
                ]
            ]
        ]
