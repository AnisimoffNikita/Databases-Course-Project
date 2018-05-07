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
            [ Grid.col [Col.md10] 
                (viewProfile model profile)
            ]
        ]


viewProfile : Model -> Profile.Profile -> List (Html Msg)
viewProfile model profile = 
    let 

        fieldView label value msg = 
            ( Grid.container []
                [ Grid.row [ Row.attrs [class "justify-content-center"] ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text label] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser msg ]
                            ]
                            [ img [src "/icons/pencil-3x.png", style [("height", "1em")]] []]
                        ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text value] ]
                    ]
                ]
            , Grid.container []
                [ Grid.row [ ]
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ] 
                        [ span [ class "text-secondary" ] [text label] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ InputGroup.config
                            ( InputGroup.text [ Input.value value ] )
                            |> InputGroup.successors
                                [ InputGroup.button [ Button.primary ] [ text "Ok"] ]
                            |> InputGroup.view
                        ] 
                    ]
                ]
            )

        (usernameView, usernameEditView) =
            fieldView "Username" profile.username Username

        (emailView, emailEditView) = 
            fieldView "Email" profile.email Email

        (passwordView, passwordEditView) =
            fieldView "Password" "" Password

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
                        [ span [] [text <| withDefault "No :(" profile.firstName] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Second name"] ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text <| withDefault "No :(" profile.secondName] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Birthday"] ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text <| formatBirthday profile.birthday ] ]
                    ]
                , Grid.row [ ]
                    [ Grid.col [ Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text "Gender"] ]
                    ]
                , Grid.row [] 
                    [ Grid.col [] 
                        [ span [] [text <| formatGender profile.gender ] ]
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
                            , Button.block
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "Ok" ]
                        ]
                    ]
                ]
        
        (v1, v2, v3, v4) = 
            case model.currentEdit of 
                Username -> 
                    ( usernameEditView
                    , emailView
                    , passwordView
                    , infoView
                    )
                Email -> 
                    ( usernameView
                    , emailEditView
                    , passwordView
                    , infoView
                    )
                Password -> 
                    ( usernameView
                    , emailView
                    , passwordEditView
                    , infoView
                    )
                Info -> 
                    ( usernameView
                    , emailView
                    , passwordView
                    , infoEditView
                    )
                NoEdit -> 
                    ( usernameView
                    , emailView
                    , passwordView
                    , infoView
                    )

        body1 =
            div [ class "text-center"
                 , class "justify-content-center"]
                [ v1
                , hr [] []
                , v2
                , hr [] []
                , v3
                ]
        body2 =
            div [ class "text-center"
                 , class "justify-content-center"]
                [ v4
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
                            , Button.block
                            , Button.attrs [Spacing.mt2]
                            ]
                            [ text "New avatar" ]
                        ]
                    |> Card.view 
                ] 
            , Grid.col []
                [ Card.config [ Card.attrs [ style [ ( "width", "100%" ) ]] ]
                    |> Card.block []
                        [ Block.custom body1
                        ]
                    |> Card.view 
                ] 

            , Grid.col []
                [ Card.config [ Card.attrs [ style [ ( "width", "100%" ) ]] ]
                    |> Card.block []
                        [ Block.custom body2
                        ]
                    |> Card.view 
                ] 
            ]
            
        ]
    ]



viewLoading : Html Msg 
viewLoading = 
    div [ class "loading"] []

viewFail : Html Msg
viewFail = 
    div
        [ class "Absolute-Center"
        ]
        [ img [ src "/Fail.png", style [("width", "50%")] ]  []]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)

formatBirthday : Maybe Date -> String 
formatBirthday bd = 
    case bd of 
        Nothing -> "No :("
        Just date -> formatDate date

formatGender : Maybe Profile.Gender -> String
formatGender g =
    case g of 
        Nothing -> "No :("
        Just Profile.Male -> "Male"
        Just Profile.Female -> "Female"