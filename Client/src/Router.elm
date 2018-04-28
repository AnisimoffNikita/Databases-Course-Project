module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser exposing ((</>))

type Route
    = Home
    | Dashboard
    | Login
    | Registration
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Dashboard (s "dashboard")
        , map Login (s "login")
        , map Registration (s "registration")
        , map NotFoundRoute (s (routeToString NotFoundRoute))
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []
                Dashboard ->
                    ["dashboard"]
                Login ->
                    ["login"]
                Registration ->
                    ["registration"]
                NotFoundRoute ->
                    ["404"]
    in
    "/" ++ String.join "/" pieces

href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)
