module Main exposing (..)

import Html exposing (..)
import Data.Session exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Router exposing (Route)
import Page.Home as Home
import Page.Dashboard as Dashboard
import Page.Login as Login
import Page.Registration as Registration
import Page.ErrorRoute as ErrorRoute
import View.Header as Header
import Ports
import Bootstrap.Grid as Grid

type alias Model =
    { session : Session
    , route : Route
    , headerModel : Header.Model
    }


init : Value-> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            Router.parseLocation location
        session = 
            case decodeSessionFromJson val of 
                Nothing -> {user = Nothing}
                Just session -> session

        (headerModel, headerCmd) = Header.init
        model = { session = session
                , route = currentRoute
                , headerModel = headerModel
                }

    in
        ( model
        , Cmd.batch 
            [  Cmd.map NavbarMsg headerCmd 
            ] 
        )


view : Model -> Html Msg
view model =
    Grid.container []
        [ Html.map NavbarMsg (Header.view model.headerModel ) 
        ]


page : Model -> Html Msg
page model =
    case model.route of
        Router.Home ->
            Home.view model.session

        Router.Dashboard ->
            Dashboard.view model.session

        Router.Login ->
            Login.view model.session

        Router.Registration ->
            Registration.view model.session

        Router.NotFoundRoute ->
            ErrorRoute.view


type Msg
    = OnLocationChange Location
    | NavigateTo String
    | NavbarMsg Header.Msg



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute = Router.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
        NavigateTo url ->
            ( model, Navigation.newUrl url )
        NavbarMsg subMsg ->
            let
                ( updatedWidgetModel, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( {model | headerModel = updatedWidgetModel}, Cmd.map NavbarMsg newCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map NavbarMsg (Header.subscriptions model.headerModel)
        ]


main : Program (Value) Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }