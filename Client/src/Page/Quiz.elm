module Page.Quiz exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Data.Tokens as Tokens exposing (..)
import Html exposing (Html, h3, text, div, p)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Http.Extra as Extra
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode 
import Navigation
import RemoteData exposing (WebData)
import Router exposing (routeToString)
import Data.Tokens exposing (Tokens)
import Data.Quiz as Quiz
import Date exposing (Date)
import List
import List.Extra as List
import String
import Debug


type alias QuestionWithoutAnswer =
    { text : String
    , variants : List (String)
    }


decodeQuestionWithoutAnswer : Decode.Decoder QuestionWithoutAnswer
decodeQuestionWithoutAnswer =
    decode QuestionWithoutAnswer
        |> required "text" Decode.string
        |> required "variants" (Decode.list Decode.string)

encodeQuestionWithoutAnswer : QuestionWithoutAnswer -> Encode.Value
encodeQuestionWithoutAnswer x =
    Encode.object
        [ ( "text", Encode.string x.text )
        , ( "variants", (Encode.list << List.map Encode.string) x.variants )
        ]

type alias QuizWithoutAnswers =
    { name : String
    , description : String
    , questions : List (QuestionWithoutAnswer)
    }

decodeQuizWithoutAnswers : Decode.Decoder QuizWithoutAnswers
decodeQuizWithoutAnswers =
    decode QuizWithoutAnswers
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "questions" (Decode.list decodeQuestionWithoutAnswer)

encodeQuizWithoutAnswers : QuizWithoutAnswers -> Encode.Value
encodeQuizWithoutAnswers x =
    Encode.object
        [ ( "name", Encode.string x.name )
        , ( "description", Encode.string x.description )
        , ( "questions", (Encode.list << List.map encodeQuestionWithoutAnswer) x.questions )
        ]

type alias Model =
    { tokens : Tokens
    , quizId : String
    , quiz : WebData QuizWithoutAnswers
    , answers : List String
    , result : WebData String
    , modalVisibility : Modal.Visibility
    }


init : Tokens -> String -> ( Model, Cmd Msg )
init tokens quizId =
    ( Model tokens quizId RemoteData.NotAsked [] RemoteData.NotAsked Modal.hidden, postQuiz tokens quizId )

postQuiz : Tokens -> String -> Cmd Msg
postQuiz tokens quizId =
    Debug.log "0" (postQuizRequest tokens quizId
        |> RemoteData.sendRequest
        |> Cmd.map QuizRecieved)


postQuizRequest : Tokens -> String -> Http.Request QuizWithoutAnswers
postQuizRequest tokens quizId =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ tokens.tokensJwt) ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = "http://localhost:8080/quiz/get/" ++ quizId
        , body = emptyBody
        , expect = Http.expectJson decodeQuizWithoutAnswers
        , timeout = Nothing
        , withCredentials = False
        }

type Msg
    = QuizRecieved (WebData QuizWithoutAnswers)
    | Choose Int String
    | Submit 
    | QuizResult (WebData String)
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.quiz)  of
        ( QuizRecieved (RemoteData.Success quiz), _ ) ->
            Debug.log "!" <| ( { model | quiz = (RemoteData.Success quiz), answers = List.repeat (List.length quiz.questions) "" }, Cmd.none )
        ( QuizRecieved _, _ ) ->
            Debug.log "!" <| ( model, Cmd.none )
        ( Choose i answer, (RemoteData.Success quiz) ) ->
            let
                answers = model.answers
                newAnswers = List.setAt i answer answers
            in
            ( {model|answers = newAnswers}, Cmd.none )
        ( Choose i choise, _ ) ->
            ( model, Cmd.none )
        (Submit, (RemoteData.Success quiz) ) -> 
            (model, postResults model)
        (Submit, _ ) -> 
            (model, Cmd.none)

        (QuizResult (RemoteData.Success ok), _) ->
            ( { model | result=(RemoteData.Success ok), modalVisibility = Modal.shown}, Cmd.none )
        (QuizResult _, _) ->
            ( model, Cmd.none )
        (CloseModal, _) -> 
            (model, Navigation.newUrl <| routeToString Router.Dashboard  )
        

postResults : Model -> Cmd Msg
postResults model =
    postResultsRequest model
        |> RemoteData.sendRequest
        |> Cmd.map QuizResult


postResultsRequest : Model -> Http.Request String
postResultsRequest model =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/quiz/result/" ++ model.quizId
        , body = Http.jsonBody ((Encode.list << List.map Encode.string) model.answers)
        , expect = Http.expectJson (Decode.string)
        , timeout = Nothing
        , withCredentials = False
        }

view : Model -> Html Msg
view model =
    case model.quiz of
        RemoteData.Success ok ->
            viewSuccess model ok

        RemoteData.Failure _ ->
            viewFail

        _ ->
            viewLoading

viewSuccess : Model -> QuizWithoutAnswers -> Html Msg
viewSuccess model quiz = 
    Grid.container
        []
        [ h3 [] [text quiz.name]
        , p [] [text quiz.description]
        , div []
            (List.indexedMap questionView quiz.questions)
        , Button.button 
            [ Button.dark 
            , Button.onClick Submit
            , Button.attrs [class "float-right"] ]
            [ text "Отправить" ]
        , viewModal model 
        ]

questionView : Int -> QuestionWithoutAnswer-> Html Msg
questionView i question =
    let 
        answerView answer = Radio.create [ Radio.inline,  Radio.onClick (Choose i answer)] answer
    in
    Grid.container
        [] 
        [ Form.group []
            [ Form.label [] [ text question.text]
            , div [] 
                (Radio.radioList (toString i)
                    (List.map answerView question.variants))
            ]
        ]


viewModal : Model -> Html Msg
viewModal model  =
    case model.result of 
        RemoteData.Success r -> 
            Modal.config CloseModal
                |> Modal.small
                |> Modal.h5 [] [ text "Результат" ]
                |> Modal.body []
                    [ text r
                    ]
                |> Modal.footer []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ onClick CloseModal ]
                        ]
                        [ text "Закрыть" ]
                    ]
                |> Modal.view model.modalVisibility
        _ -> div [] []


viewLoading : Html Msg
viewLoading =
    div [ class "загрузка" ] []


viewFail : Html Msg
viewFail =
    div [ class "ошибка" ] []