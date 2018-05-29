module Page.Dashboard exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing
import Data.Profile as Profile
import Data.Tokens exposing (..)
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
import Html exposing (Html, a, div, h1, h3, h4, h5, hr, img, p, span, text, small, input, label)
import Html.Attributes exposing (class, href, src, style, type_, id, for, attribute, multiple)
import Html.Events exposing (onClick, on)
import Http exposing (..)
import Http.Extra as Extra
import Json.Encode as Encode
import Json.Decode as Decode
import Maybe exposing (withDefault)
import RemoteData exposing (WebData)
import Date.Extra exposing (toUtcIsoString)
import Ports exposing (ImagePortData, fileSelected, fileContentRead)
import FileReader exposing (NativeFile)
import FileReader.FileDrop as DZ
import MimeType
import Router 

type Edit
    = Username
    | Email
    | Password
    | Info
    | NoEdit

type alias Image =
    { contents : String
    , filename : String
    }


type alias Model =
    { profile : WebData Profile.Profile
    , password : String
    , currentEdit : Edit
    , datePicker : DatePicker.DatePicker
    , modalVisibility : Modal.Visibility
    , errorMessage : String
    , tokens : Tokens
    , file : Maybe NativeFile
    }


settings : DatePicker.Settings
settings =
    { defaultSettings
        | isDisabled = \_ -> False
        , inputClassList = [ ( "form-control", True ) ]
        , inputName = Just "date"
        , inputId = Just "date-field"
    }


init : Tokens -> ( Model, Cmd Msg )
init tokens =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( Model RemoteData.NotAsked "" NoEdit datePicker Modal.hidden "" tokens Nothing 
    , Cmd.batch
        [ getProfile tokens
        , Cmd.map InputBirthday datePickerFx
        ]
    )


getProfile : Tokens -> Cmd Msg
getProfile tokens =
    postProfile tokens
        |> RemoteData.sendRequest
        |> Cmd.map ProfileRecieved


postProfile : Tokens -> Http.Request Profile.Profile
postProfile tokens =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ tokens.tokensJwt) ]
    in
    Http.request
        { method = "GET"
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

    | InputUsername String
    | SubmitUsername
    | SetUsername (WebData Tokens)

    | InputEmail String
    | SubmitEmail
    | SetEmail (Result Http.Error Extra.NoContent)

    | InputPassword String
    | SubmitPassword
    | SetPassword (Result Http.Error Extra.NoContent)

    | InputFirstName String
    | InputSecondName String
    | InputBirthday DatePicker.Msg
    | InputGender String
    | SubmitInfo
    | SetInfo (Result Http.Error Extra.NoContent)

    | CloseModal
    | ShowModal


    | AvatarSelected (List NativeFile)
    | SubmitAvatar
    | SetAvatar (Result Http.Error String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.profile ) of
        ( ProfileRecieved profile, _ ) ->
            ( { model | profile = profile }, Cmd.none )

        ( EditUser edit, _ ) ->
            ( { model | currentEdit = edit }, Cmd.none )


        ( InputUsername username, RemoteData.Success profile ) ->
            let
                newProfile =
                    { profile | username = username }
            in
            ( { model | profile = RemoteData.succeed newProfile }, Cmd.none )

        ( SubmitUsername, RemoteData.Success profile ) ->
            ( model, setUsernameCommand model profile.username )

        ( SetUsername (RemoteData.Success tokens), _ ) ->
            ( { model | currentEdit = NoEdit, tokens = tokens }, Cmd.none )

        ( SetUsername _, _ ) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = "error" }, Cmd.none )


        ( InputEmail email, RemoteData.Success profile ) ->
            let
                newProfile =
                    { profile | email = email }
            in
            ( { model | profile = RemoteData.succeed newProfile }, Cmd.none )

        ( SubmitEmail, RemoteData.Success profile ) ->
            ( model, setEmailCommand model profile.email )

        ( SetEmail (Ok Extra.NoContent), _ ) ->
            ( { model | currentEdit = NoEdit }, Cmd.none )

        ( SetEmail (Err error), _ ) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = toString error }, Cmd.none )


        ( InputPassword password, RemoteData.Success _ ) ->
            ( { model | password = password }, Cmd.none )

        ( SubmitPassword, RemoteData.Success _ ) ->
            ( model, setPasswordCommand model model.password )

        ( SetPassword (Ok Extra.NoContent), _ ) ->
            ( { model | currentEdit = NoEdit, password = "" }, Cmd.none )

        ( SetPassword (Err error), _ ) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = toString error }, Cmd.none )


        ( InputFirstName firstName, RemoteData.Success profile ) ->
            let
                mFirstName = if firstName == "" then Nothing else Just firstName
                newProfile =
                    { profile | firstName = mFirstName }
            in
            ( { model | profile = RemoteData.succeed newProfile }, Cmd.none )

        ( InputSecondName secondName, RemoteData.Success profile ) ->
            let
                mSecondName = if secondName == "" then Nothing else Just secondName
                newProfile =
                    { profile | secondName = mSecondName }
            in
            ( { model | profile = RemoteData.succeed newProfile }, Cmd.none )

        ( InputBirthday msg, RemoteData.Success profile ) ->
            let
                ( newDatePicker, datePickerFx, mDate ) =
                    DatePicker.update settings msg model.datePicker

                date =
                    case mDate of
                        DatePicker.Changed date ->
                            date

                        _ ->
                            profile.birthday

                newProfile =
                    { profile | birthday = date }
            in
            ( { model | datePicker = newDatePicker, profile = RemoteData.succeed newProfile }, Cmd.none )

        ( InputGender gender, RemoteData.Success profile ) ->
            let
                newProfile =
                    { profile | gender = toGender gender }
            in
            ( { model | profile = RemoteData.succeed newProfile }, Cmd.none )

        ( SubmitInfo, RemoteData.Success profile ) ->
            ( model, setInfoCommand model profile )

        ( SetInfo (Ok Extra.NoContent), _ ) ->
            ( { model | currentEdit = NoEdit }, Cmd.none )

        ( SetInfo (Err error), _ ) ->
            ( { model | modalVisibility = Modal.shown, errorMessage = toString error}, Cmd.none )

        ( CloseModal, _ ) ->
            ( { model | modalVisibility = Modal.hidden, errorMessage = "" }
            , Cmd.none
            )

        ( AvatarSelected file, _ ) ->
            case file of
                [ f ] ->
                    case f.mimeType of 
                        Just (MimeType.Image _) ->
                            ( { model | file = Just f}, setAvatarCommand model f)
                        _ -> ( model, Cmd.none )
                _ ->
                    ( model, Cmd.none )
                    
        ( SubmitAvatar, RemoteData.Success profile ) ->
            ( model, Cmd.none)

        ( SetAvatar (Ok path),  RemoteData.Success profile ) ->
            let
                newProfile =
                    { profile | avatar = path }
            in
            ( { model | profile = RemoteData.succeed newProfile}, Cmd.none )

        ( SetAvatar (Err error), _ ) ->
            ( { model | file = Nothing, modalVisibility = Modal.shown, errorMessage = toString error }, Cmd.none )


        ( _, _ ) ->
            ( model, Cmd.none )


setUsernameCommand : Model -> String -> Cmd Msg
setUsernameCommand model username =
    createSetUsernameRequest model username
        |> RemoteData.sendRequest
        |> Cmd.map SetUsername


createSetUsernameRequest : Model -> String -> Http.Request Tokens
createSetUsernameRequest model username =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/username"
        , body = Http.jsonBody (Encode.string username)
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
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/email"
        , body = Http.jsonBody (Encode.string email)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }

setPasswordCommand : Model -> String -> Cmd Msg
setPasswordCommand model password =
    createSetPasswordRequest model password
        |> Http.send SetPassword


createSetPasswordRequest : Model -> String -> Http.Request Extra.NoContent
createSetPasswordRequest model password =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/password"
        , body = Http.jsonBody (Encode.string password)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }

type alias UserInfo =
    { firstName : Maybe String
    , secondName : Maybe String
    , birthday : Maybe Date
    , gender : Maybe Profile.Gender
    }

setInfoCommand : Model -> Profile.Profile -> Cmd Msg
setInfoCommand model profile =
    createSetInfoRequest model profile
        |> Http.send SetInfo


createSetInfoRequest : Model -> Profile.Profile -> Http.Request Extra.NoContent
createSetInfoRequest model profile =
    let
        userInfo = UserInfo profile.firstName profile.secondName profile.birthday profile.gender
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/profile"
        , body = Http.jsonBody (encodeUserInfo userInfo)
        , expect = Extra.expectNoContent
        , timeout = Nothing
        , withCredentials = False
        }

setAvatarCommand : Model -> NativeFile -> Cmd Msg
setAvatarCommand model avatar =
    createSetAvatarRequest model avatar
        |> Http.send SetAvatar


createSetAvatarRequest : Model -> NativeFile -> Http.Request String
createSetAvatarRequest model avatar =
    let
        headers =
            [ header "Authorization" ("Bearer " ++ model.tokens.tokensJwt) ]
        body =
            Http.multipartBody
                [ Http.stringPart "part1" avatar.name
                , FileReader.filePart "upload" avatar
                ]

    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "http://localhost:8080/user/edit/avatar"
        , body = body
        , expect = Http.expectJson (Decode.string)
        , timeout = Nothing
        , withCredentials = False
        }

encodeUserInfo : UserInfo -> Encode.Value
encodeUserInfo x =
    Encode.object
        [ ( "firstname", (Maybe.withDefault Encode.null << Maybe.map Encode.string) x.firstName )
        , ( "secondname", (Maybe.withDefault Encode.null << Maybe.map Encode.string) x.secondName )
        , ( "birthday", (Maybe.withDefault Encode.null << Maybe.map (Encode.string << toUtcIsoString)) x.birthday )
        , ( "gender", (Maybe.withDefault Encode.null << Maybe.map Profile.encodeGender) x.gender )
        ]

view : Model -> Html Msg
view model =
    case model.profile of
        RemoteData.Success ok ->
            viewSuccess model ok

        RemoteData.Failure _ ->
            viewFail

        _ ->
            viewLoading


viewModal : Model -> Html Msg
viewModal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h5 [] [ text "Что-то пошло не так" ]
        |> Modal.body []
            [ text model.errorMessage
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view model.modalVisibility


viewSuccess : Model -> Profile.Profile -> Html Msg
viewSuccess model profile =
    Grid.container []
        [ Grid.row [ Row.attrs [ Spacing.mt2, class "justify-content-center" ] ]
            [ Grid.col [ Col.md9 ]
                (viewProfile model profile)
            ]
        , viewModal model
        ]


viewProfile : Model -> Profile.Profile -> List (Html Msg)
viewProfile model profile =
    let
        fieldView label value msg msgInput msgSubmit input showValue =
            ( Grid.container []
                [ Grid.row [ Row.attrs [ class "justify-content-center" ] ]
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text label ] ]
                    , Grid.col [ Col.md2, Col.offsetMd1 ]
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.onClick <| EditUser msg
                            ]
                            [ img [ src "/icons/pencil-3x.png", style [ ( "height", "1em" ) ] ] [] ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ span [] [text <| if showValue then  value else "" ] ]
                    ]
                ]
            , Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text label ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ InputGroup.config
                            (input [ Input.value value, Input.onInput msgInput ])
                            |> InputGroup.successors
                                [ InputGroup.button [ Button.primary, Button.onClick msgSubmit ] [ text "Ok" ] ]
                            |> InputGroup.view
                        ]
                    ]
                ]
            )

        ( usernameView, usernameEditView ) =
            fieldView "Логин" profile.username Username InputUsername SubmitUsername InputGroup.text True

        ( emailView, emailEditView ) =
            fieldView "Email" profile.email Email InputEmail SubmitEmail InputGroup.email True

        ( passwordView, passwordEditView ) =
            fieldView "Пароль" model.password Password InputPassword SubmitPassword InputGroup.password False

        infoView =
            Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.md6, Col.offsetMd3, Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Имя" ] ]
                    , Grid.col [ Col.md2, Col.offsetMd1 ]
                        [ Button.button
                            [ Button.roleLink
                            , Button.small
                            , Button.attrs [ onClick <| EditUser Info ]
                            ]
                            [ img [ src "/icons/pencil-3x.png", style [ ( "height", "1em" ) ] ] [] ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ span [] [ text <| withDefault "" profile.firstName ] ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Фамилия" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ span [] [ text <| withDefault "" profile.secondName ] ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Дата рождения" ] ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ small [ class "text-secondary" ] [ text "День" ] ]
                    , Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ small [ class "text-secondary" ] [ text "Месяц" ] ]
                    , Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ small [ class "text-secondary" ] [ text "Год" ] ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ ] [ text <| withDefault "" <| Maybe.map (day >> toString) profile.birthday ] ]
                    , Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ ] [ text <| withDefault "" <| Maybe.map (month >> toString) profile.birthday ] ]
                    , Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ ] [ text <| withDefault "" <| Maybe.map (year >> toString) profile.birthday ] ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Пол" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ span [] [ text <| formatGender profile.gender ] ]
                    ]
                ]

        infoEditView =
            Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Имя" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ Input.email [ Input.onInput InputFirstName,  Input.value <| withDefault "" profile.firstName ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Фамилия" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ Input.email [ Input.onInput InputSecondName, Input.value <| withDefault "" profile.secondName ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Дата рождения" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ DatePicker.view profile.birthday settings model.datePicker
                            |> Html.map InputBirthday
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.attrs [ Spacing.my1 ] ]
                        [ span [ class "text-secondary" ] [ text "Пол" ] ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ Select.custom [Select.onChange InputGender]
                            [ Select.item [] [ text "" ]
                            , Select.item [] [ text "Мужской" ]
                            , Select.item [] [ text "Женский" ]
                            ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col []
                        [ Button.button
                            [ Button.primary
                            , Button.block
                            , Button.onClick SubmitInfo
                            , Button.attrs [ Spacing.mt2 ]
                            ]
                            [ text "Применить" ]
                        ]
                    ]
                ]

        ( v1, v2, v3, v4 ) =
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

        body =
            div
                [ class "text-center"
                , class "justify-content-center"
                ]
                [ v1
                , hr [] []
                , v2
                , hr [] []
                , v3
                , hr [] []
                , v4
                ]

    in
    [ Grid.container [ Border.all, Border.rounded ]
        [ Grid.row []
            [ Grid.col [ Col.attrs [ Spacing.p2, Spacing.m1 ] ]
                [ img
                    [ src <| "http://localhost:8080/" ++ profile.avatar
                    , style [ ( "width", "100%" ) ]
                    ]
                    []
                , div 
                    [ class "custom-file" ]
                    [ 
                     label [ class "btn btn-block btn-outline-dark mt-2" ]
                        [ text "Изменить"
                        , input 
                            [ class "custom-file-input"
                            , attribute "hidden" ""
                            , type_ "file"
                            , FileReader.onFileChange AvatarSelected
                            ]
                            []
                        ]
                    ]
                ]
            , Grid.col [ Col.attrs [ Spacing.p2, Spacing.m1, Border.left, Border.right ] ]
                [ body
                ]
            , Grid.col [ Col.attrs [ Spacing.p2, Spacing.m1 ] ]
                [ Button.linkButton 
                    [ Button.outlineDark 
                    , Button.block
                    , Button.attrs [ href <| Router.routeToString Router.Quizies]]
                    [ text "Мои тесты" ]
                , Button.linkButton 
                    [ Button.outlineDark 
                    , Button.block
                    , Button.attrs [ href "#"]]
                    [ text "Пройденные тесты" ]
                ]
            ]
        ]
    ]


viewLoading : Html Msg
viewLoading =
    div [ class "loading" ] []


viewFail : Html Msg
viewFail =
    div [ class "error" ] []


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


formatBirthday : Maybe Date -> String
formatBirthday bd =
    case bd of
        Nothing ->
            ""

        Just date ->
            formatDate date


formatGender : Maybe Profile.Gender -> String
formatGender g =
    case g of
        Nothing ->
            ""

        Just Profile.Male ->
            "Мужской"

        Just Profile.Female ->
            "Женский"

toGender : String -> Maybe Profile.Gender
toGender g = 
    case g of 
        "" -> Nothing 
        "Мужской" -> Just Profile.Male
        "Женский" -> Just Profile.Female
        _ -> Nothing

