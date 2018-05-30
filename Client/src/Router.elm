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
    | Quizies
    | NewQuiz
    | Quiz String
    | Search String
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Dashboard (s "dashboard")
        , map Login (s "login")
        , map Register (s "register")
        , map Quizies (s "quizies" </> s "my")
        , map NewQuiz (s "quizies" </> s "new")
        , map Quiz (s "quiz" </> string)
        , map Search (s "search" </> string)
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

                Quizies ->
                    [ "quizies", "my" ]

                NewQuiz ->
                    [ "quizies", "new" ]

                Quiz id ->
                    [ "quiz", id ]

                Search query ->
                    [ "search", query ]

                NotFoundRoute ->
                    [ "404" ]
    in
    "/#/" ++ String.join "/" pieces



-- href_ : Route -> Attribute msg
-- href_ route =
--     Attr.href (routeToString route)
