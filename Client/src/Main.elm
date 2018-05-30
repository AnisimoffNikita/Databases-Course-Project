module Main exposing (..)

import Bootstrap.Grid as Grid
import Data.Session exposing (..)
import Data.Tokens as Tokens exposing (..)
import Data.User as User exposing (User)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Page.Dashboard as Dashboard
import Page.ErrorRoute as ErrorRoute
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Page.Quizies as Quizies
import Page.NewQuiz as NewQuiz
import Page.Quiz as Quiz
import Page.Search as Search
import Ports
import RemoteData exposing (WebData)
import Router exposing (Route)
import View.Header as Header
import Date
import Task

type Page
    = Home
    | Login Login.Model
    | Register Register.Model
    | Dashboard Dashboard.Model
    | Quizies Quizies.Model
    | Quiz Quiz.Model
    | NewQuiz NewQuiz.Model
    | Search Search.Model


type alias Model =
    { session : Session
    , route : Route
    , headerModel : Header.Model
    , page : Page
    , date : Maybe Date.Date
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            Router.parseLocation location

        session =
            case decodeSessionFromJson val of
                Nothing ->
                    { tokens = Nothing }

                Just session ->
                    session

        ( headerModel, headerCmd ) =
            Header.init

        ( model, cmds ) =
            setRoute currentRoute
                { session = session
                , route = currentRoute
                , headerModel = headerModel
                , page = Home
                , date = Nothing
                }
    in
    ( model
    , Cmd.batch
        [ Cmd.map HeaderMsg headerCmd
        , cmds
        , now
        ]
    )


view : Model -> Html Msg
view model =
    let
        content =
            case model.page of
                Home ->
                    div
                        [ class "jumbotron"
                        , class "jumbotron-fluid"
                        ]
                        [ Grid.container []
                            [ h1 [] [ text "Quizzes" ]
                            , p [] [ text "Make it. Pass it. Share it." ]
                            ]
                        ]

                Login loginModel ->
                    Html.map LoginMsg Login.view

                Register regModel ->
                    Html.map RegisterMsg Register.view

                Dashboard subModel ->
                    Html.map DashboardMsg <| Dashboard.view subModel

                Quizies subModel ->
                    Html.map QuiziesMsg <| Quizies.view subModel

                Quiz subModel ->
                    Html.map QuizMsg <| Quiz.view subModel

                NewQuiz subModel ->
                    Html.map NewQuizMsg <| NewQuiz.view subModel

                Search subModel ->
                    Html.map SearchMsg <| Search.view subModel

    in
    div []
        [ Html.map HeaderMsg (Header.view model.session model.headerModel)
        , content
        ]


type Msg
    = OnLocationChange Location
    | NavigateTo String
    | GetDate (Maybe Date.Date)
    | HeaderMsg Header.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | DashboardMsg Dashboard.Msg
    | QuiziesMsg Quizies.Msg
    | NewQuizMsg NewQuiz.Msg
    | QuizMsg Quiz.Msg
    | SearchMsg Search.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( OnLocationChange location, _ ) ->
            let
                newRoute =
                    Router.parseLocation location
            in
            setRoute newRoute model
        (GetDate date, _ )->
            ( {model|date = date}, Cmd.none )

        ( HeaderMsg Header.Logout, _ ) ->
            let
                session =
                    { tokens = Nothing }
            in
            ( { model | session = session }
            , Cmd.batch
                [ Navigation.modifyUrl <| Router.routeToString Router.Home
                , Ports.setStorage session
                ]
            )

        ( HeaderMsg subMsg, _ ) ->
            let
                ( updated, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( { model | headerModel = updated }, Cmd.map HeaderMsg newCmd )

        ( LoginMsg (Login.TokensRecieved user (RemoteData.Success tokens)), Login subModel ) ->
            let
                session =
                    Session <| Just tokens
            in
            ( { model | session = session }
            , Cmd.batch
                [ Navigation.modifyUrl <| Router.routeToString Router.Home
                , Ports.setStorage session
                ]
            )

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ( updated, newCmd ) =
                    Login.update subMsg subModel
            in
            ( { model | page = Login updated }, Cmd.map LoginMsg newCmd )

        ( RegisterMsg (Register.TokensRecieved user (RemoteData.Success tokens)), Register subModel ) ->
            let
                session =
                    Session <| Just tokens
            in
            ( { model | session = session }
            , Cmd.batch
                [ Navigation.modifyUrl <| Router.routeToString Router.Home
                , Ports.setStorage session
                ]
            )

        ( RegisterMsg subMsg, Register subModel ) ->
            let
                ( updated, newCmd ) =
                    Register.update subMsg subModel
            in
            ( { model | page = Register updated }, Cmd.map RegisterMsg newCmd )

        ( DashboardMsg (Dashboard.SetUsername (RemoteData.Success tokens)), Dashboard subModel ) ->
            let
                session =
                    Session <| Just tokens

                ( updated, newCmd ) =
                    Dashboard.update (Dashboard.SetUsername (RemoteData.Success tokens)) subModel
            in
            ( { model | session = session, page = Dashboard updated }
            , Cmd.batch
                [ Ports.setStorage session
                , Cmd.map DashboardMsg newCmd
                ]
            )

        ( DashboardMsg subMsg, Dashboard subModel ) ->
            let
                ( updated, newCmd ) =
                    Dashboard.update subMsg subModel
            in
            ( { model | page = Dashboard updated }, Cmd.map DashboardMsg newCmd )

        ( QuiziesMsg subMsg, Quizies subModel ) ->
            let
                ( updated, newCmd ) =
                    Quizies.update subMsg subModel
            in
            ( { model | page = Quizies updated }, Cmd.map QuiziesMsg newCmd )

        ( NewQuizMsg subMsg, NewQuiz subModel ) ->
            let
                ( updated, newCmd ) =
                    NewQuiz.update subMsg subModel
            in
            ( { model | page = NewQuiz updated }, Cmd.map NewQuizMsg newCmd )


        ( SearchMsg subMsg, Search subModel ) ->
            let
                ( updated, newCmd ) =
                    Search.update subMsg subModel
            in
            ( { model | page = Search updated }, Cmd.map SearchMsg newCmd )

        ( _, _ ) ->
            ( model, Cmd.none )


setRoute : Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    case model.session.tokens of
        Just tokens ->
            sessionOk route model tokens

        Nothing ->
            sessionBad route model


sessionOk : Route -> Model -> Tokens -> ( Model, Cmd Msg )
sessionOk route model tokens =
    case route of
        Router.Home ->
            ( { model | page = Home }, Cmd.none )

        Router.Login ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Dashboard )

        Router.Register ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Dashboard )

        Router.Quizies ->
            let
                ( subModel, msg ) =
                    Quizies.init tokens
            in
            (  { model | page = Quizies subModel }, Cmd.map QuiziesMsg msg )
        
        Router.NewQuiz ->
            case model.date of 
                Just date ->
                    let
                        ( subModel, msg ) =
                            NewQuiz.init tokens date
                    in
                    (  { model | page = NewQuiz subModel }, Cmd.map NewQuizMsg msg )
                _ -> ( model, Navigation.modifyUrl <| Router.routeToString Router.Dashboard )

        Router.Quiz id ->
            let
                ( subModel, msg ) =
                    Quiz.init tokens id
            in
            (  { model | page = Quiz subModel }, Cmd.map QuizMsg msg )

        Router.Dashboard ->
            let
                ( subModel, msg ) =
                    Dashboard.init tokens
            in
            ( { model | page = Dashboard subModel }, Cmd.map DashboardMsg msg )

        
        Router.Search query ->
            let
                ( subModel, msg ) =
                    Search.init tokens query
            in
            ( { model | page = Search subModel }, Cmd.map SearchMsg msg )

        Router.NotFoundRoute ->
            ( model, Cmd.none )


sessionBad : Route -> Model -> ( Model, Cmd Msg )
sessionBad route model =
    case route of
        Router.Home ->
            ( { model | page = Home }, Cmd.none )

        Router.Login ->
            let
                ( subModel, msg ) =
                    Login.init
            in
            ( { model | page = Login subModel }, Cmd.map LoginMsg msg )

        Router.Register ->
            let
                ( subModel, msg ) =
                    Register.init
            in
            ( { model | page = Register subModel }, Cmd.map RegisterMsg msg )

        Router.Quizies ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.NewQuiz ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.Quiz id ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.Dashboard ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.Search query ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.NotFoundRoute ->
            ( model, Cmd.none )

now : Cmd Msg
now = 
  Task.perform (Just >> GetDate) Date.now

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map HeaderMsg (Header.subscriptions model.headerModel)
        ]


main : Program Value Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

