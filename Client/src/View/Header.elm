module View.Header exposing (..)

import Html exposing (Html, Attribute, div, text, a, p, img, h1)
import Html.Attributes exposing (href, src, class, style, placeholder)
import Data.Session exposing (..)
import Router exposing (..)
import Html.Events.Extra exposing (onClickPreventDefault)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Size as Size
import Navigation

import Bootstrap.Navbar as Navbar

-- You need to keep track of the view state for the navbar in your model

type alias Model =
    { navbarState : Navbar.State }


-- The navbar needs to know the initial window size, so the inital state for a navbar requires a command to be run by the Elm runtime

init : (Model, Cmd Msg)
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ({ navbarState = navbarState }, navbarCmd)


-- Define a message for the navbar

type Msg
    = NavbarMsg Navbar.State
    | NavigateTo String


-- You need to handle navbar messages in your update function to step the navbar state forward

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )
        NavigateTo url ->
            ( model, Navigation.newUrl url )



subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg


view : Model -> Html Msg
view model = 
    div []
    [ Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseSmall
        |> Navbar.container
        |> Navbar.attrs [class "bg-white"]
        |> Navbar.brand                     -- Add logo to your brand with a little styling to align nicely
            [ href "#" ] [text " Quizzy"]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Button.button
                    [ Button.primary
                    , Button.attrs 
                        [ Spacing.ml2Sm
                        , href (routeToString Registration)
                        , onClickPreventDefault (NavigateTo <| routeToString Registration)] 
                    ]
                    [ text "Register"]
                ]
            , Navbar.textItem [ Spacing.ml2Lg, class "muted" ] [ text "or"]
            , Navbar.formItem []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs 
                        [ Spacing.ml2Sm
                        , href (routeToString Login)
                        , onClickPreventDefault (NavigateTo <| routeToString Login)] 
                    ]
                    [ text "Login"]
                ]
            ]
        |> Navbar.view model.navbarState
    ]
    