module Page.Dashboard exposing (..)

import Html exposing (Html, div, text, img, h3, h4, h5, hr, p, span, a)
import Html.Attributes exposing (src, class, style, href)
import Html.Events exposing (onClick)
import Http exposing (..)
import Data.Tokens exposing (..)
import Data.Profile as Profile
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Border as Border
import Bootstrap.Button as Button
import Bootstrap.Form.Select as Select
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Modal as Modal
import Bootstrap.Form.InputGroup as InputGroup
import DatePicker exposing (defaultSettings)
import RemoteData exposing (WebData)
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import Debug
import Maybe exposing (withDefault)
import Http.Extra as Extra
import Json.Encode as Encode

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
    , modalVisibility : Modal.Visibility
    , errorMessage : String
    , tokens : Tokens
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
    ( Model RemoteData.NotAsked "" NoEdit datePicker Modal.hidden "" tokens
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
    | InputUsername String
    | SubmitUsername
    | SetUsername (WebData Tokens)
    | InputEmail String
    | SubmitEmail
    | SetEmail (Result Http.Error Extra.NoContent)
    
    | CloseModal
    | ShowModal

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model.profile) of 
        (ProfileRecieved profile, _) -> 
            ({model | profile = profile}, Cmd.none)
        (EditUser edit, _) -> 
            ({model | currentEdit = edit}, Cmd.none)
        (DatePickerMsg msg, RemoteData.Success profile) ->
            let
                ( newDatePicker, datePickerFx, mDate ) =
                    DatePicker.update settings msg model.datePicker

                date =
                    Debug.log "!!" <| case mDate of
                        DatePicker.Changed date ->
                            date
                        _ ->
                            profile.birthday   

                newProfile = {profile | birthday = date}                      
            in
            ({ model | datePicker = newDatePicker, profile = RemoteData.succeed newProfile}, Cmd.none )
        (InputUsername username, RemoteData.Success profile) ->
            let
                newProfile = {profile | username = username}                      
            in
            ({ model | profile = RemoteData.succeed newProfile }, Cmd.none )
        (SubmitUsername, RemoteData.Success profile) ->
            ( model, setUsernameCommand model profile.username )
        (SetUsername (RemoteData.Success tokens), _) ->
            ( {model | currentEdit = NoEdit, tokens = tokens}, Cmd.none )
        (SetUsername _, _) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = "error" }, Cmd.none )
        (InputEmail email, RemoteData.Success profile) ->
            let
                newProfile = {profile | email = email}                      
            in
            ({ model | profile = RemoteData.succeed newProfile }, Cmd.none )
        (SubmitEmail, RemoteData.Success profile) ->
            ( model, setEmailCommand model profile.email )
        (SetEmail (Ok Extra.NoContent), _) ->
            ( {model | currentEdit = NoEdit}, Cmd.none )
        (SetEmail (Err _), _) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = "error" }, Cmd.none )

        (CloseModal, _) ->
            ( { model | modalVisibility = Modal.hidden, errorMessage = "" }
            , Cmd.none
            )
        (_, _) ->
            ( model, Cmd.none )

setUsernameCommand : Model -> String -> Cmd Msg
setUsernameCommand model username =
    createSetUsernameRequest model username
        |> RemoteData.sendRequest
        |> Cmd.map SetUsername

createSetUsernameRequest : Model -> String -> Http.Request Tokens
createSetUsernameRequest model username =
    let 
        headers = [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt)]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/username"
        , body =  Http.jsonBody (Encode.string username)
        , expect = Http.expectJson decodeTokens
        , timeout = Nothing
        , withCredentials = False
        }   


setEmailCommand : Model -> String -> Cmd Msg
setEmailCommand model email =
    createSetEmailRequest model email
        |> Http.send SetEmail

createSetEmailRequest : Model -> String -> Http.Request Extra.NoContent
createSetEmailRequest model email =
    let 
        headers = [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt)]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/email"
        , body =  Http.jsonBody (Encode.string email)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }   

view : Model -> Html Msg
view model = 
    case model.profile of 
        RemoteData.Success ok -> viewSuccess model ok
        RemoteData.Failure _ -> viewFail
        _ -> viewLoading

viewModal : Model -> Html Msg
viewModal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h5 [] [ text "Something went wrong" ]
        |> Modal.body []
            [ text model.errorMessage
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Close" ]
            ]
        |> Modal.view model.modalVisibility
        

viewSuccess : Model -> Profile.Profile -> Html Msg
viewSuccess model profile = 
    Grid.container []
        [ Grid.row [ Row.attrs [Spacing.mt2, class "justify-content-center"] ]
            [ Grid.col [Col.md9] 
                (viewProfile model profile)
            ]
        , viewModal model
        ]


viewProfile : Model -> Profile.Profile -> List (Html Msg)
viewProfile model profile = 
    let 

        fieldView label value msg msgInput msgSubmit = 
            ( Grid.container []
                [ Grid.row [ Row.attrs [class "justify-content-center"] ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [Spacing.my1] ] 
                        [ span [ class "text-secondary" ] [text label] ]
                    , Grid.col [Col.md2, Col.offsetMd1 ] 
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.onClick <| EditUser msg 
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
                            ( InputGroup.text [ Input.value value, Input.onInput msgInput ] )
                            |> InputGroup.successors
                                [ InputGroup.button [ Button.primary, Button.onClick msgSubmit ] [ text "Ok"] ]
                            |> InputGroup.view
                        ] 
                    ]
                ]
            )

        (usernameView, usernameEditView) =
            fieldView "Username" profile.username Username InputUsername SubmitUsername

        (emailView, emailEditView) = 
            fieldView "Email" profile.email Email InputEmail SubmitEmail

        (passwordView, passwordEditView) =
            fieldView "Password" "" Password InputUsername SubmitUsername

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
                        [ (DatePicker.view profile.birthday settings model.datePicker)
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
    [ Grid.container [Border.all, Border.rounded]
        [ Grid.row [  ]
            [ Grid.col [ Col.attrs [ Spacing.p2, Spacing.m1] ]
                [  img  
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

            , Grid.col [ Col.attrs [ Spacing.p2, Spacing.m1, Border.left, Border.right ] ]
                [ body1
                ] 

            ,Grid.col [ Col.attrs [  Spacing.p2, Spacing.m1 ] ]
                [ body2
                ] 
            ]
            
        ]
    ]



viewLoading : Html Msg 
viewLoading = 
    div [ class "loading"] []

viewFail : Html Msg
viewFail = 
    div [ class "error"] []


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