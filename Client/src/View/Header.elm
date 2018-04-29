module View.Header exposing (..)

import Html exposing (Html, Attribute, div, text, a, p, img)
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


-- You need to handle navbar messages in your update function to step the navbar state forward

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


-- view : Model -> Html Msg
-- view model =
--     Navbar.config NavbarMsg
--         |> Navbar.withAnimation
--         |> Navbar.brand [ href "#"] [ text "Brand"]
--         |> Navbar.items
--             [ Navbar.itemLink [href "#"] [ text "Item 1"]
--             , Navbar.itemLink [href "#"] [ text "Item 2"]
--             ]
--         |> Navbar.view model.navbarState


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg


view : Model -> Html Msg
view model = 
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.fixTop                      -- Customize coloring
        |> Navbar.collapseSmall
        |> Navbar.brand                     -- Add logo to your brand with a little styling to align nicely
            [ href "#" ]
            [ img
                [ src "elm-bootstrap.svg"
                , class "d-inline-block"
                , style [ ( "width", "30px" ), ("margin", "0px") ]
                ]
                []
            , text " Elm Bootstrap"
            ]
        |> Navbar.items
            [ Navbar.itemLink
                [ href "#" ] [ text "Item 1" ]
            , Navbar.dropdown              -- Adding dropdowns is pretty simple
                { id = "mydropdown"
                , toggle = Navbar.dropdownToggle [] [ text "My dropdown" ]
                , items =
                    [ Navbar.dropdownHeader [ text "Heading" ]
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 1" ]
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 2" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#" ]
                        [ text "Drop item 3" ]
                    ]
                }
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Input.text [ Input.attrs [placeholder "enter" ]]
                , Button.button
                    [ Button.success
                    , Button.attrs [ Spacing.ml2Sm]
                    ]
                    [ text "Search"]
                ]
            , Navbar.textItem [ Spacing.ml2Lg, class "muted" ] [ text "Text"]
            ]
        |> Navbar.view model.navbarState
    