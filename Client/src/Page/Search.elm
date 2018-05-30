module Page.Search exposing (..)

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
    , query : String
    , quizies : WebData (List QuizPreview)
    }


init : Tokens -> String -> ( Model, Cmd Msg )
init tokens query =
    ( Model tokens query RemoteData.NotAsked, createGetTestsCommand tokens query )


type Msg
    = QuiziesRecieved (WebData (List QuizPreview))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuiziesRecieved quizies -> 
            ({model|quizies = quizies}, Cmd.none)



createGetTestsCommand : Tokens -> String -> Cmd Msg
createGetTestsCommand tokens query =
    getQuiziesRequest tokens query
        |> RemoteData.sendRequest 
        |> Cmd.map QuiziesRecieved


getQuiziesRequest : Tokens -> String -> Http.Request (List QuizPreview)
getQuiziesRequest tokens query =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ tokens.tokensJwt) ]
    in
    Http.request
        { method =
            "GET"
        , headers = headers
        , url = "http://localhost:8080/quiz/search/"++query
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
