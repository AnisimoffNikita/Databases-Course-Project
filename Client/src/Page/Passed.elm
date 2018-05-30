module Page.Passed exposing (..)

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


type alias QuizPreviewResult =
    { name : String
    , description : String
    , result : String
    , id : String
    }

decodeQuizPreview : Decode.Decoder QuizPreviewResult
decodeQuizPreview =
    decode QuizPreviewResult
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "result" Decode.string
        |> required "id" Decode.string

encodeQuizPreview : QuizPreviewResult -> Encode.Value
encodeQuizPreview x =
    Encode.object
        [ ( "name", Encode.string x.name )
        , ( "description", Encode.string x.description )
        , ( "result", Encode.string x.result )
        , ( "id", Encode.string x.id )
        ]

type alias Model =
    { tokens : Tokens
    , quizies : WebData (List QuizPreviewResult)
    }


init : Tokens -> ( Model, Cmd Msg )
init tokens =
    ( Model tokens RemoteData.NotAsked, createGetTestsCommand tokens )


type Msg
    = QuiziesRecieved (WebData (List QuizPreviewResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuiziesRecieved quizies -> 
            ({model|quizies = quizies}, Cmd.none)



createGetTestsCommand : Tokens -> Cmd Msg
createGetTestsCommand tokens =
    getQuiziesRequest tokens
        |> RemoteData.sendRequest 
        |> Cmd.map QuiziesRecieved


getQuiziesRequest : Tokens -> Http.Request (List QuizPreviewResult)
getQuiziesRequest tokens =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ tokens.tokensJwt) ]
    in
    Http.request
        { method =
            "GET"
        , headers = headers
        , url = "http://localhost:8080/quiz/get/passed/"
        , body = emptyBody
        , expect =
            Http.expectJson (Decode.list decodeQuizPreview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }





view : Model -> Html Msg
view model =
    case model.quizies of 
        RemoteData.Success quizies ->
            Grid.container [class "card-columns"]
                (List.indexedMap previewQuiz quizies)
        _ -> div [] [text "ошибка"]

previewQuiz : Int -> QuizPreviewResult -> Html Msg
previewQuiz i quiz = 
    Card.config [ Card.outlineSecondary ]
        |> Card.headerH4 [] [ text quiz.name ]
        |> Card.block []
            [ Block.text [] [ text quiz.description ]
            , Block.text [] [ text quiz.result ]
            , Block.custom <|
                Form.row []
                    [ Form.col [ Col.md12 ] 
                        [ Button.linkButton 
                                [ Button.outlinePrimary
                                , Button.block
                                , Button.attrs [href <| "/#/quiz/" ++ quiz.id ]
                                ] [ text "Пройти!" ]
                        ]
                    ]

            ]
        |> Card.view
