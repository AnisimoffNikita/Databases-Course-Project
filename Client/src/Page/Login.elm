module Page.Login exposing (..)

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


type alias LoginData =
    { username : String
    , password : String
    }


decodeLogin : Decoder LoginData
decodeLogin =
    decode LoginData
        |> required "username" string
        |> required "password" string


encodeLogin : LoginData -> Encode.Value
encodeLogin x =
    Encode.object
        [ ( "username", Encode.string x.username )
        , ( "password", Encode.string x.password )
        ]


type alias Model =
    { username : String
    , password : String
    , tokens : WebData Tokens
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" RemoteData.NotAsked, Cmd.none )


type Msg
    = Login
    | Register
    | TokensRecieved LoginData (WebData Tokens)
    | Username String
    | Password String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            ( model, createPostCommand model )

        Register ->
            ( model, Navigation.newUrl <| routeToString Router.Register )

        TokensRecieved user response ->
            ( { model | tokens = response }, Cmd.none )

        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )


createPostCommand : Model -> Cmd Msg
createPostCommand model =
    let
        body =
            { username = model.username
            , password = model.password
            }
    in
    postUserLogin body
        |> RemoteData.sendRequest
        |> Cmd.map (TokensRecieved body)


postUserLogin : LoginData -> Http.Request Tokens
postUserLogin body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url = "http://localhost:8080/user/login"
        , body =
            Http.jsonBody (encodeLogin body)
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
            Form.form [ onSubmit Login ]
                [ Form.group []
                    [ Form.label [ for "myusername" ] [ text "Username" ]
                    , Input.text [ Input.id "myusername", Input.attrs [ onInput Username ] ]
                    ]
                , Form.group []
                    [ Form.label [ for "mypwd" ] [ text "Password" ]
                    , Input.password [ Input.id "mypwd", Input.attrs [ onInput Password ] ]
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
                [ h1 [] [ text "Login" ]
                , Button.linkButton
                    [ Button.roleLink
                    , Button.attrs [ href <| routeToString Router.Register ]
                    ]
                    [ text "No account?" ]
                , form
                ]
            ]
        ]
