module View.Header exposing (..)

import Html exposing (Html, Attribute, div, text, a, p, img, h1, button, form)
import Html.Attributes exposing (href, src, class, style, placeholder)
import Html.Events exposing (onClick, onSubmit)
import Data.Session exposing (..)
import Router exposing (Route, routeToString)
import Html.Events.Extra exposing (onClickPreventDefault)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Size as Size
import Navigation
import Utils exposing (href_)

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
    | Logout
    | Search


-- You need to handle navbar messages in your update function to step the navbar state forward

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )
        NavigateTo url ->
            ( model, Navigation.newUrl url )
        Logout ->
            ( model, Cmd.none)
        Search ->
            ( model, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



view : Session -> Model -> Html Msg
view session model = 
    let 
        controls = 
            case session.tokens of 
                Nothing -> 
                    Navbar.customItems
                        [ Navbar.formItem [onSubmit <| NavigateTo <| routeToString Router.Register]
                            [ Button.button
                                [ Button.primary
                                , Button.attrs 
                                    [ Spacing.ml2Sm
                                    ] 
                                ]
                                [ text "Register"]
                            ]
                        , Navbar.formItem [onSubmit <| NavigateTo <| routeToString Router.Login]
                            [ Button.button 
                                [ Button.outlinePrimary
                                , Button.attrs 
                                    [ Spacing.ml2Sm
                                    ] 
                                ]
                                [ text "Login"]
                            ]
                        ]
                Just user -> 
                    Navbar.customItems
                        [ Navbar.formItem [onSubmit Search]
                            [ InputGroup.config
                                ( InputGroup.text [ Input.placeholder "Search for" ] )
                                |> InputGroup.successors
                                    [ InputGroup.button [ Button.primary ] [ text "Search"] ]
                                |> InputGroup.view
                            ] 
                        , Navbar.formItem [onSubmit <| NavigateTo <| routeToString Router.Dashboard]
                            [ Button.button
                                [ Button.primary
                                , Button.attrs 
                                    [ Spacing.ml2Sm
                                    ] 
                                ]
                                [ text "Dashboard"]
                            ]
                        , Navbar.formItem [onSubmit Logout]
                            [ Button.button 
                                [ Button.outlinePrimary
                                , Button.attrs 
                                    [ Spacing.ml2Sm
                                    ] 
                                ]
                                [ text "Log out"]
                            ]
                        ]

    in
    div []
    [ Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseSmall
        |> Navbar.container
        |> Navbar.attrs [class "bg-white"]
        |> Navbar.brand                     -- Add logo to your brand with a little styling to align nicely
            [ href "/#" ] [text " Quizzy"]
        |> controls
        |> Navbar.view model.navbarState
    ]
    