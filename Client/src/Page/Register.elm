module Page.Register exposing (..)

import Html exposing (Html, text, h1)
import Html.Attributes exposing (for, class, href)
import Html.Events exposing (onClick, onSubmit, onInput)
import Http exposing (..)
import Router exposing (routeToString)
import Navigation

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import RemoteData exposing (WebData)
import Json.Decode exposing (Decoder,string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value, object)
import Data.Tokens as Tokens exposing (..)


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


init : (Model, Cmd Msg)
init =
        (Model "" "" "" RemoteData.NotAsked, Cmd.none)

type Msg
    = Register
    | Login
    | TokensRecieved UserRegister (WebData Tokens)
    | Username String
    | Email String
    | Password String 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Register ->
            ( model, createPostCommand model )
        Login ->
            ( model, Navigation.newUrl <| routeToString Router.Login )
        TokensRecieved user response ->
            ( {model | tokens = response}, Cmd.none)
        Username username ->
            ( {model | username = username}, Cmd.none)
        Email email ->
            ( {model | email = email}, Cmd.none)
        Password password ->
            ( {model | password = password}, Cmd.none)


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
        , url = "http://localhost:8080/user/new"
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
        Form.form [onSubmit Register]
            [ Form.group []
                [ Form.label [for "myemail"] [ text "Email address"]
                , Input.email [ Input.id "myemail", Input.attrs [onInput Email] ]
                ]
            , Form.group []
                [ Form.label [for "myusername"] [ text "Username"]
                , Input.text [ Input.id "myusername", Input.attrs [onInput Username] ]
                ]
            , Form.group []
                [ Form.label [for "mypwd"] [ text "Password"]
                , Input.password [ Input.id "mypwd", Input.attrs [onInput Password] ]
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