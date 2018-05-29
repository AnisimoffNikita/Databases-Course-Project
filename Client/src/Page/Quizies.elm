module Page.Quizies exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ButtonGroup as ButtonGroup
import Data.Tokens as Tokens exposing (..)
import Html exposing (Html, h1, text, div)
import Html.Attributes exposing (class, for, href)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Json.Decode as Decode 
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value, object)
import Navigation
import RemoteData exposing (WebData)
import Router exposing (routeToString)
import Data.Tokens exposing (Tokens)
import Http.Extra as Extra


type alias QuizPreview =
    { name : String
    , description : String
    , passingNumber : Int
    , id : String
    }

decodeQuizPreview : Decode.Decoder QuizPreview
decodeQuizPreview =
    decode QuizPreview
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "passingNumber" Decode.int
        |> required "id" Decode.string

encodeQuizPreview : QuizPreview -> Encode.Value
encodeQuizPreview x =
    Encode.object
        [ ( "name", Encode.string x.name )
        , ( "description", Encode.string x.description )
        , ( "passingNumber", Encode.int x.passingNumber )
        , ( "id", Encode.string x.id )
        ]

type alias Model =
    { tokens : Tokens
    , quizies : WebData (List QuizPreview)
    }


init : Tokens -> ( Model, Cmd Msg )
init tokens =
    ( Model tokens RemoteData.NotAsked, createGetTestsCommand tokens )


type Msg
    = QuiziesRecieved (WebData (List QuizPreview))
    | Remove String
    | Removed (Result Http.Error Extra.NoContent)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuiziesRecieved quizies -> 
            ({model|quizies = quizies}, Cmd.none)
        Remove id ->
            ( model, setRemoveCommand model id )

        Removed (Ok Extra.NoContent) ->
            ( model, createGetTestsCommand model.tokens )

        Removed (Err error) ->
            ( model, Cmd.none )



createGetTestsCommand : Tokens -> Cmd Msg
createGetTestsCommand tokens =
    getQuiziesRequest tokens
        |> RemoteData.sendRequest 
        |> Cmd.map QuiziesRecieved


getQuiziesRequest : Tokens -> Http.Request (List QuizPreview)
getQuiziesRequest tokens =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ tokens.tokensJwt) ]
    in
    Http.request
        { method =
            "GET"
        , headers = headers
        , url = "http://localhost:8080/quiz/get/user"
        , body = emptyBody
        , expect =
            Http.expectJson (Decode.list decodeQuizPreview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }



setRemoveCommand : Model -> String -> Cmd Msg
setRemoveCommand model id =
    createRemoveRequest model id
        |> Http.send Removed


createRemoveRequest : Model -> String -> Http.Request Extra.NoContent
createRemoveRequest model id =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/quiz/remove"
        , body = Http.jsonBody (Encode.string id)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }

view : Model -> Html Msg
view model =
    let 
        new =
            Card.config [ Card.outlinePrimary ]
                |> Card.block []
                    [ Block.custom <|
                        Button.linkButton 
                            [ Button.primary
                            , Button.block 
                            , Button.attrs [href  <| routeToString Router.NewQuiz ]
                            ] [ text "Создать новый тест" ]
                    ]
                |> Card.view
    in
    case model.quizies of 
        RemoteData.Success quizies ->
            Grid.container [class "card-columns"]
                (List.indexedMap previewQuiz quizies ++ [new])
        _ -> div [] [text "ошибка"]

previewQuiz : Int -> QuizPreview -> Html Msg
previewQuiz i quiz = 
    Card.config [ Card.outlineSecondary ]
        |> Card.headerH4 [] [ text quiz.name ]
        |> Card.block []
            [ Block.text [] [ text quiz.description ]
            , Block.custom <|
                Form.row []
                    [ Form.col [ Col.md8 ] 
                        [ Button.linkButton 
                                [ Button.outlinePrimary
                                , Button.block
                                , Button.attrs [href <| "/#/quiz/" ++ quiz.id ]
                                ] [ text "Пройти!" ]
                        ]
                    , Form.col [ Col.md4 ]
                        [ Button.button
                            [ Button.outlineDanger
                            , Button.onClick (Remove quiz.id)
                            ]
                            [ text "Удалить" ] 
                        ]
                    ]

            ]
        |> Card.view
