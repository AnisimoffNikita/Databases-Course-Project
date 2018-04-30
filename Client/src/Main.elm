module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
import Page.Login as Login


type Page  
    = Home 
    | Login Login.Model

type alias Model =
    { session : Session
    , route : Route
    , headerModel : Header.Model
    , page : Page
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
                , page = Home
                }

    in
        ( model
        , Cmd.batch 
            [  Cmd.map NavbarMsg headerCmd 
            ] 
        )


view : Model -> Html Msg
view model =
    let content = 
        case model.page of 
            Home -> 
                div 
                    [ class "jumbotron"
                    , class "jumbotron-fluid"] 
                    [ Grid.container [] 
                        [ h1  [] [text "Quizes"]
                        , p [] [text "Make it. Pass it. Share it."]]

                    ]
            Login loginModel -> 
                 Html.map LoginMsg Login.view
    in
        div []
        [ Html.map NavbarMsg (Header.view model.headerModel ) 
        , content
        ]




type Msg
    = OnLocationChange Location
    | NavigateTo String
    | NavbarMsg Header.Msg
    | LoginMsg Login.Msg



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
        LoginMsg subMsg ->
            -- let

            --     ( update, newCmd ) =
            --         Login.update subMsg model.
            -- in
            ( model, Cmd.none)

updatePage : Route -> Model -> Model
updatePage

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