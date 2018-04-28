module Main exposing (..)

import Html exposing (..)
import Data.Session exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Router exposing (Route)
import Msgs exposing (..)
import Page.Home as Home
import Page.Dashboard as Dashboard
import Page.Login as Login
import Page.Registration as Registration
import Page.ErrorRoute as ErrorRoute
import Page.Index exposing (index)
import Ports

type alias Model =
    { session : Session
    , route : Route
    }


init : Value-> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            Router.parseLocation location
        model = 
            case decodeSessionFromJson val of 
                Nothing -> {session = {user = Nothing}, route = currentRoute}
                Just session -> {session = session, route = currentRoute}
    in
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    model
      |> page
      |> index model.session


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



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnLocationChange location ->
            let
                newRoute = Router.parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )
        Msgs.NavigateTo url ->
            ( model, Navigation.newUrl url )



main : Program (Value) Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }