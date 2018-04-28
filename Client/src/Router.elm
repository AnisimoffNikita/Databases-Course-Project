module Router exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Home
    | Dashboard
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Dashboard (s "dashboard")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute

