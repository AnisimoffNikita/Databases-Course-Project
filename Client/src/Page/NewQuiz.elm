module Page.NewQuiz exposing (..)

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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (..)
import Http.Extra as Extra
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value, object)
import Navigation
import RemoteData exposing (WebData)
import Router exposing (routeToString)
import Data.Tokens exposing (Tokens)
import Data.Quiz as Quiz
import Date exposing (Date)
import List
import List.Extra as List
import String

type alias Model =
    { tokens : Tokens
    , quiz : Quiz.Quiz
    }


init : Tokens -> Date -> ( Model, Cmd Msg )
init tokens date =
    let 
        quiz = Quiz.Quiz "" "" date 0 []
    in
    ( Model tokens quiz, Cmd.none )


type Msg
    = NoOp
    | NewQuestion
    | InputName String 
    | InputDescription String
    | InputQuestionName Int String
    | InputQuestionVariants Int String
    | RemoveQuestion Int
    | Submit 
    | Response (Result Http.Error Extra.NoContent)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        NewQuestion -> 
            let 
                quiz = model.quiz
                newQuestions = quiz.questions ++ [Quiz.Question "" "" []]
                newQuiz = {quiz | questions = newQuestions}
            in 
            ( {model | quiz = newQuiz}, Cmd.none)
        InputName name -> 
            let 
                quiz = model.quiz 
                newQuiz = {quiz | name = name}
            in
            ({model | quiz = newQuiz}, Cmd.none)
        InputDescription description ->
            let 
                quiz = model.quiz 
                newQuiz = {quiz | description = description}
            in
            ({model | quiz = newQuiz}, Cmd.none)
        InputQuestionName i name -> 
            let 
                quiz = model.quiz 
                questions = quiz.questions
                newQuestions = case List.getAt i questions of 
                    Just question -> List.setAt i {question | text = name} questions
                    Nothing -> questions
                newQuiz = {quiz | questions = newQuestions}
            in
            ({model | quiz = newQuiz}, Cmd.none)
        InputQuestionVariants i text -> 
            let 
                quiz = model.quiz 
                questions = quiz.questions
                variants = String.split "\n" text
                answer = Maybe.withDefault "" (List.head variants)
                newQuestions = case List.getAt i questions of 
                    Just question -> List.setAt i {question | variants = variants, answer = answer} questions
                    Nothing -> questions
                newQuiz = {quiz | questions = newQuestions}
            in
            ({model | quiz = newQuiz}, Cmd.none)
        RemoveQuestion i -> 
            let 
                quiz = model.quiz 
                questions = quiz.questions
                newQuestions = List.removeAt i questions
                newQuiz = {quiz | questions = newQuestions}
            in
            ({model | quiz = newQuiz}, Cmd.none)
        Submit ->
            ( model, sendQuizCommand model)

        Response (Ok _) ->
            ( model,  Navigation.newUrl <| routeToString Router.Quizies )

        Response (Err error)->
            ( Debug.log (toString error) model, Cmd.none )




sendQuizCommand : Model -> Cmd Msg
sendQuizCommand model =
    createSendQuizRequest model
        |> Http.send Response


createSendQuizRequest : Model -> Http.Request Extra.NoContent
createSendQuizRequest model =
    let
        headers =
            [ Http.header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/quiz/new"
        , body = Http.jsonBody (Quiz.encodeQuiz model.quiz)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }

view : Model -> Html Msg
view model =
    let 
        questionsData = model.quiz.questions
        dataLen = List.length questionsData
        questions = div 
            [] <| 
            List.indexedMap questionView questionsData
    in
    Grid.container
        []
        [ h3 [] [text "Новый тест"]
        , Form.group []
            [ Form.label [] [ text "Название"]
            , Input.text [ Input.onInput InputName ]
            ]
        , Form.group []
            [ Form.label [] [ text "Описание"]
            , Textarea.textarea
                [ Textarea.rows 3
                , Textarea.onInput InputDescription 
                ]
            ]
        , questions
        , Button.button 
            [ Button.outlineDark
            , Button.onClick NewQuestion ]
            [ text "Добавить новый вопрос" ]
        , Button.button 
            [ Button.dark 
            , Button.onClick Submit
            , Button.attrs [class "float-right"] ]
            [ text "Сохранить" ]
        ]

questionView : Int -> Quiz.Question -> Html Msg
questionView i question =
    Grid.container
        [] 
        [ Form.group []
            [ Form.label [] [ text "Вопрос"]
            , Button.button 
                [ Button.outlineDark
                , Button.small 
                , Button.attrs [class "float-right"]
                , Button.onClick (RemoveQuestion i) ]
                [ text "удалить" ]
            , Input.text [ Input.value question.text, Input.onInput (InputQuestionName i) ]
            , Form.label [] [ text "Варианты ответа (первый должен быть правильным)"]
            , Textarea.textarea
                [ Textarea.rows 3
                , Textarea.value <| String.join "\n" question.variants
                , Textarea.onInput (InputQuestionVariants i)
                ]
            ]
        ]



