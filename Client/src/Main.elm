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
import Ports
import RemoteData exposing (WebData)
import Router exposing (Route)
import View.Header as Header


type Page
    = Home
    | Login Login.Model
    | Register Register.Model
    | Dashboard Dashboard.Model


type alias Model =
    { session : Session
    , route : Route
    , headerModel : Header.Model
    , page : Page
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
                }
    in
    ( model
    , Cmd.batch
        [ Cmd.map HeaderMsg headerCmd
        , cmds
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
    in
    div []
        [ Html.map HeaderMsg (Header.view model.session model.headerModel)
        , content
        ]


type Msg
    = OnLocationChange Location
    | NavigateTo String
    | HeaderMsg Header.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | DashboardMsg Dashboard.Msg


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

        Router.Dashboard ->
            let
                ( subModel, msg ) =
                    Dashboard.init tokens
            in
            ( { model | page = Dashboard subModel }, Cmd.map DashboardMsg msg )

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

        Router.Dashboard ->
            ( model, Navigation.modifyUrl <| Router.routeToString Router.Login )

        Router.NotFoundRoute ->
            ( model, Cmd.none )


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
