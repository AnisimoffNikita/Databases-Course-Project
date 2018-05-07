module Router exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Home
    | Dashboard
    | Login
    | Register
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Dashboard (s "dashboard")
        , map Login (s "login")
        , map Register (s "register")
        , map NotFoundRoute (s (routeToString NotFoundRoute))
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
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
                    [ "dashboard" ]

                Login ->
                    [ "login" ]

                Register ->
                    [ "register" ]

                NotFoundRoute ->
                    [ "404" ]
    in
    "/#/" ++ String.join "/" pieces



-- href_ : Route -> Attribute msg
-- href_ route =
--     Attr.href (routeToString route)
