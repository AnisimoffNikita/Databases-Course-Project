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
import Page.Register as Register
import Page.ErrorRoute as ErrorRoute
import View.Header as Header
import Ports
import Bootstrap.Grid as Grid
import Page.Login as Login


type Page  
    = Home 
    | Login Login.Model
    | Register Register.Model

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
            [ Cmd.map NavbarMsg headerCmd 
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
            Register regModel -> 
                 Html.map RegisterMsg Register.view
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
    | RegisterMsg Register.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model

updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case (msg, page) of 
        (OnLocationChange location, _) ->
            let
                newRoute = Router.parseLocation location
            in
            setRoute newRoute model

        -- (NavigateTo url, _) ->
        --     ( model, Navigation.newUrl url )
        (NavbarMsg subMsg, _) ->
            let
                ( updated, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( {model | headerModel = updated}, Cmd.map NavbarMsg newCmd )
        (LoginMsg subMsg, Login subModel) ->
            let
                ( updated, newCmd ) =
                    Login.update subMsg subModel
            in
            ( {model | page = Login updated}, Cmd.map LoginMsg newCmd)

        (RegisterMsg subMsg, Register subModel) ->
            let
                ( updated, newCmd ) =
                    Register.update subMsg subModel
            in
            ( {model | page = Register updated}, Cmd.map RegisterMsg newCmd)

        (_, _) -> 
            ( model, Cmd.none ) 



setRoute : Route -> Model -> (Model, Cmd Msg)
setRoute route model = 
    case route of 
        Router.Home -> 
            ({model | page = Home}, Cmd.none)
        Router.Login -> 
            let 
                (subModel, msg) = Login.init 
            in
            ({model | page = Login subModel}, Cmd.map LoginMsg msg)
        Router.Register -> 
            let 
                (subModel, msg) = Register.init 
            in
            ({model | page = Register subModel}, Cmd.map RegisterMsg msg)
        Router.Dashboard -> 
            (model, Cmd.none)
        Router.NotFoundRoute -> 
            (model, Cmd.none)





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