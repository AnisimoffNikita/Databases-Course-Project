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
import Bootstrap.Form.Select as Select
import Bootstrap.Text as Text
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Progress as Progress
import DatePicker exposing (defaultSettings)
import RemoteData exposing (WebData)
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
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
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    }


settings : DatePicker.Settings
settings =
        { defaultSettings
            | isDisabled = \_->False
            , inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
            , inputId = Just "date-field"
        }

init : Tokens -> (Model, Cmd Msg)
init tokens =
    let
        ( datePicker, datePickerFx ) =
                DatePicker.init
    in
    ( Model RemoteData.NotAsked "" NoEdit datePicker Nothing
    , Cmd.batch 
        [ getProfile tokens
        , Cmd.map DatePickerMsg datePickerFx
        ]
    )


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
    | DatePickerMsg DatePicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ProfileRecieved profile -> 
            ({model | profile = profile}, Cmd.none)
        EditUser edit -> 
            ({model | currentEdit = edit}, Cmd.none)
        DatePickerMsg msg ->
            let
                ( newDatePicker, datePickerFx, mDate ) =
                    DatePicker.update settings msg model.datePicker

                date =
                    case mDate of
                        DatePicker.Changed date ->
                            date
                        _ ->
                            model.date                            
            in
            ({ model | datePicker = newDatePicker, date = date}, Cmd.none )


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
            [ Grid.col [Col.md7] 
                (viewProfile model profile)
            ]
        ]


viewProfile : Model -> Profile.Profile -> List (Html Msg)
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
                        [ span [ class "text-secondary" ] [text "Password"] ]
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
                        [ span [ class "text-secondary" ] [text "Date"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ (DatePicker.view model.date settings model.datePicker)
                            |> Html.map DatePickerMsg
                        ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text "Gender"] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [] 
                        [ 
                            Select.custom []
                                [ Select.item [] [ text ""]
                                , Select.item [] [ text "Male"]
                                , Select.item [] [ text "Female"]
                                ]
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
    [ Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Card.config [ Card.attrs [ style [ ( "width", "100%" ) ]] ]
                    |> Card.header [ class "text-center"]
                        [ img 
                            [ src <| "http://localhost:8080/"++profile.avatar
                            , style [ ( "width", "100%" ) ]
                            ] [] 
                        , Button.button
                            [ Button.primary
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "New avatar" ]
                        ]
                    |> Card.view 
                ] 
            , Grid.col []
                [ Card.config [ Card.attrs [ style [ ( "width", "100%" ) ]] ]
                    |> Card.block []
                        [ Block.custom body
                        ]
                    |> Card.view 
                ] 
            ]
        ]
    ]



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


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)