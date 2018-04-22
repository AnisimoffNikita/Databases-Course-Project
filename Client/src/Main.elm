port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main =
  Html.programWithFlags
    { init =  init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , token : String
  }


init : Maybe Model -> ( Model, Cmd Msg )
init model =
    case model of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            (Model "id121" "123456" "", Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE

type Msg
    = Name String
    | Password String
    | GetToken
    | NewToken (Result Http.Error String)
    | RemoveToken

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Name name ->
      ({ model | name = name }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)

    GetToken ->
      (model, getToken model)

    NewToken (Ok newToken) ->
      setStorageHelper { model | token = newToken }

    NewToken (Err _) ->
      (model, Cmd.none)

    RemoveToken ->
      ( { model | token = "" }, removeStorage model )

getToken : Model -> Cmd Msg
getToken model =
  let
    url =
      "http://localhost:8080/user/login"

    request = authUser model url
  in
    Http.send NewToken request


loginEncoder : Model -> Encode.Value
loginEncoder model = 
    Encode.object 
        [ ("loginUsername", Encode.string model.name)
        , ("loginPassword", Encode.string model.password) 
        ]  

authUser : Model -> String -> Http.Request String
authUser model apiUrl =
    let
        body =
            model
                |> loginEncoder
                |> Http.jsonBody
    in
      Http.request
        { body = body
        , expect = Http.expectJson tokenDecoder
        , headers = defaultRequestHeaders
        , method = "POST"
        , timeout = Nothing
        , url = apiUrl
        , withCredentials = False
        }


defaultRequestHeaders : List Http.Header
defaultRequestHeaders =
    [ Http.header  "Access-Control-Request-Method" "POST"
    , Http.header  "Access-Control-Request-Headers" "X-Custom-Header"
    ]


tokenDecoder : Decode.Decoder String
tokenDecoder =
    Decode.field "tokensJwt" Decode.string    



port setStorage : Model -> Cmd msg


port removeStorage : Model -> Cmd msg


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage model )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name, value model.name ] []
    , input [ type_ "password", placeholder "Password", onInput Password, value model.password ] []
    , button [ onClick GetToken ] [ text "get token" ]
    , button [ onClick RemoveToken ] [ text "remove token" ]
    , div [ ] [text model.token]
    ]
