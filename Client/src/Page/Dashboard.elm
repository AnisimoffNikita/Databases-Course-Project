module Page.Dashboard exposing (..)

import Html exposing (Html, div, text, img, h3, h4, h5, hr, p, span, a)
import Html.Attributes exposing (src, class, style, href)
import Html.Events exposing (onClick)
import Http exposing (..)
import Data.Session exposing (..)
import Data.Tokens exposing (..)
import Data.Profile as Profile
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.Text as Text
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Progress as Progress
import RemoteData exposing (WebData)
import Debug
import Maybe exposing (withDefault)

type Edit 
    = Username
    | Email
    | Password
    | Info
    | NoEdit

type alias Model = 
    { profile : WebData Profile.Profile
    , password : String
    , currentEdit : Edit
    }

init : Tokens -> (Model, Cmd Msg)
init tokens =
    (Model RemoteData.NotAsked "" NoEdit, getProfile tokens)


getProfile : Tokens -> Cmd Msg
getProfile tokens =
    postProfile tokens
        |> RemoteData.sendRequest
        |> Cmd.map ProfileRecieved

postProfile : Tokens -> Http.Request (Profile.Profile)
postProfile tokens =
    let 

        headers = [ header "Authorization" ("Bearer " ++ tokens.tokensJwt)]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/profile"
        , body = emptyBody
        , expect = Http.expectJson Profile.decodeProfile
        , timeout = Nothing
        , withCredentials = False
        }

type Msg 
    = ProfileRecieved (WebData Profile.Profile)
    | EditUser Edit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ProfileRecieved profile -> 
            ({model | profile = profile}, Cmd.none)
        EditUser edit -> 
            ({model | currentEdit = edit}, Cmd.none)


view : Model -> Html Msg
view model = 
    case model.profile of 
        RemoteData.Success ok -> viewSuccess model ok
        RemoteData.Failure _ -> viewFail
        _ -> viewLoading



viewSuccess : Model -> Profile.Profile -> Html Msg
viewSuccess model profile = 
    Grid.container []
        [ Grid.row [ Row.attrs [Spacing.mt2, class "justify-content-center"] ]
            [ Grid.col [Col.md3] 
                [ viewProfile model profile ]
            ]
        ]


viewProfile : Model -> Profile.Profile -> Html Msg
viewProfile model profile = 
    let 

        usernameView =
            Grid.container []
                [ Grid.row [ Row.attrs [class "justify-content-center"] ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Username"] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser Username ]
                            ]
                            [ img [src "/icons/pencil-3x.png", style [("height", "1em")]] []]
                        ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text profile.username] ]
                    ]
                ]

        usernameEditView =
            Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Username"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.text [ Input.value profile.username]
                        ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Button.button
                            [ Button.primary
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "Ok" ]
                        ]
                    ]
                ]

        emailView =
            Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Email"] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser Email ]
                            ]
                            [ img [src "/icons/pencil-3x.png", style [("height", "1em")]] []]
                        ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text profile.email] ]
                    ]
                ]

        emailEditView =
            Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Email"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.text [ Input.value profile.email]
                        ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Button.button
                            [ Button.primary
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "Ok" ]
                        ]
                    ]
                ]


        passwordView =
            Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Password"] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser Password ]
                            ]
                            [ img [src "/icons/pencil-3x.png", style [("height", "1em")]] []]
                        ]
                    ]
                ]

        passwordEditView =
            Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Email"] ]
                    ]
                , Grid.row [ Row.attrs [class "justify-content-center"] ]
                    [ Grid.col [] 
                        [ Input.password [ ]
                        ]
                    ]
                , Grid.row [  ]
                    [ Grid.col [] 
                        [ Button.button
                            [ Button.primary
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "Ok" ]
                        ]
                    ]
                ]


        infoView =
            Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "First name"] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser Info ]
                            ]
                            [ img [src "/icons/pencil-3x.png", style [("height", "1em")]] []]
                        ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text <| withDefault "" profile.firstName] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Second name"] ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text <| withDefault "" profile.secondName] ]
                    ]
                ]

        infoEditView =
            Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "First name"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.email [ Input.value <| withDefault "" profile.firstName]
                        ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Second name"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.email [ Input.value <| withDefault "" profile.secondName ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "First name"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.email [ Input.value <| withDefault "" profile.firstName]
                        ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Second name"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ Input.email [ Input.value <| withDefault "" profile.secondName ]
                        ]
                    ]
                , Grid.row [  ]
                    [ Grid.col [] 
                        [ Button.button
                            [ Button.primary
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "Ok" ]
                        ]
                    ]
                ]

        body =
            case model.currentEdit of 
                Username -> 
                    div [ class "text-center"
                        , class "justify-content-center"]
                        [ usernameEditView
                        , hr [] []
                        , emailView
                        , hr [] []
                        , passwordView
                        , hr [] []
                        , infoView
                        ]
                Email -> 
                    div [ class "text-center"
                        , class "justify-content-center"]
                        [ usernameView
                        , hr [] []
                        , emailEditView
                        , hr [] []
                        , passwordView
                        , hr [] []
                        , infoView
                        ]
                Password -> 
                    div [ class "text-center"
                        , class "justify-content-center"]
                        [ usernameView
                        , hr [] []
                        , emailView
                        , hr [] []
                        , passwordEditView
                        , hr [] []
                        , infoView
                        ]
                Info -> 
                    div [ class "text-center"
                        , class "justify-content-center"]
                        [ usernameView
                        , hr [] []
                        , emailView
                        , hr [] []
                        , passwordView
                        , hr [] []
                        , infoEditView
                        ]
                NoEdit -> 
                    div [ class "text-center"
                        , class "justify-content-center"]
                        [ usernameView
                        , hr [] []
                        , emailView
                        , hr [] []
                        , passwordView
                        , hr [] []
                        , infoView
                        ]
            
    in
    Card.config [ Card.attrs [ style [ ( "width", "100%" ) ] ] ]
        |> Card.header []
            [ img 
                [ src <| "http://localhost:8080/"++profile.avatar
                , style [ ( "width", "100%" ) ]
                ] [] ]

        |> Card.block []
            [ Block.custom body
            ]
        |> Card.view

viewLoading : Html Msg 
viewLoading = 
    Grid.container 
        [ class "Absolute-Center"
        , class "is-Responsive"
        ]
        [ Progress.progress 
            [ Progress.info
            , Progress.value 100
            , Progress.animated
            ]
        ]

viewFail : Html Msg
viewFail = 
    div
        [ class "Absolute-Center"
        , class "is-Responsive"
        ]
        [ img [ src "http://localhost:8080/Fail.png", style [("width", "50%")] ]  []]