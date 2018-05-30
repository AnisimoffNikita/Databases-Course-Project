module View.Header exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Data.Session exposing (..)
import Html exposing (Attribute, Html, a, button, div, form, h1, img, p, text)
import Html.Attributes exposing (class, href, placeholder, src, style)
import Html.Events exposing (onClick, onSubmit, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Navigation
import Router exposing (Route, routeToString)
import Utils exposing (href_)


-- You need to keep track of the view state for the navbar in your model


type alias Model =
    { navbarState : Navbar.State
    , query : String }



-- The navbar needs to know the initial window size, so the inital state for a navbar requires a command to be run by the Elm runtime


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { navbarState = navbarState, query = "" }, navbarCmd )



-- Define a message for the navbar


type Msg
    = NavbarMsg Navbar.State
    | NavigateTo String
    | Logout
    | Input String



-- You need to handle navbar messages in your update function to step the navbar state forward


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        NavigateTo url ->
            ( model, Navigation.newUrl url )

        Logout ->
            ( model, Cmd.none )

        Input query ->
            ( {model | query = query}, Cmd.none )


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
                        [ Navbar.formItem [ onSubmit <| NavigateTo <| routeToString Router.Register ]
                            [ Button.button
                                [ Button.primary
                                , Button.attrs
                                    [ Spacing.ml2Sm
                                    ]
                                ]
                                [ text "Регистрация" ]
                            ]
                        , Navbar.formItem [ onSubmit <| NavigateTo <| routeToString Router.Login ]
                            [ Button.button
                                [ Button.outlinePrimary
                                , Button.attrs
                                    [ Spacing.ml2Sm
                                    ]
                                ]
                                [ text "Авторизация" ]
                            ]
                        ]

                Just user ->
                    Navbar.customItems
                        [ Navbar.formItem [  onSubmit <| NavigateTo <| routeToString (Router.Search model.query)  ]
                            [ InputGroup.config
                                (InputGroup.text [Input.onInput Input])
                                |> InputGroup.successors
                                    [ InputGroup.button [ Button.primary ] [ text "Поиск" ] ]
                                |> InputGroup.view
                            ]
                        , Navbar.formItem [ onSubmit <| NavigateTo <| routeToString Router.Dashboard ]
                            [ Button.button
                                [ Button.primary
                                , Button.attrs
                                    [ Spacing.ml2Sm
                                    ]
                                ]
                                [ text "Панель управления" ]
                            ]
                        , Navbar.formItem [ onSubmit Logout ]
                            [ Button.button
                                [ Button.outlinePrimary
                                , Button.attrs
                                    [ Spacing.ml2Sm
                                    ]
                                ]
                                [ text "Выход" ]
                            ]
                        ]
    in
    div []
        [ Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseSmall
            |> Navbar.container
            |> Navbar.attrs [ class "bg-white" ]
            |> Navbar.brand
                -- Add logo to your brand with a little styling to align nicely
                [ href "/#" ]
                [ text " Quizzy" ]
            |> controls
            |> Navbar.view model.navbarState
        ]
