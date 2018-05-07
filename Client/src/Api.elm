module Api exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type NoContent
    = NoContent


type alias Login =
    { loginUsername : String
    , loginPassword : String
    }


decodeLogin : Decoder Login
decodeLogin =
    decode Login
        |> required "loginUsername" string
        |> required "loginPassword" string


encodeLogin : Login -> Json.Encode.Value
encodeLogin x =
    Json.Encode.object
        [ ( "loginUsername", Json.Encode.string x.loginUsername )
        , ( "loginPassword", Json.Encode.string x.loginPassword )
        ]


type alias UserRegister =
    { registerUsername : String
    , registerEmail : String
    , registerPassword : String
    }


decodeUserRegister : Decoder UserRegister
decodeUserRegister =
    decode UserRegister
        |> required "registerUsername" string
        |> required "registerEmail" string
        |> required "registerPassword" string


encodeUserRegister : UserRegister -> Json.Encode.Value
encodeUserRegister x =
    Json.Encode.object
        [ ( "registerUsername", Json.Encode.string x.registerUsername )
        , ( "registerEmail", Json.Encode.string x.registerEmail )
        , ( "registerPassword", Json.Encode.string x.registerPassword )
        ]


type alias Tokens =
    { tokensJwt : String
    }


decodeTokens : Decoder Tokens
decodeTokens =
    decode Tokens
        |> required "tokensJwt" string


encodeTokens : Tokens -> Json.Encode.Value
encodeTokens x =
    Json.Encode.object
        [ ( "tokensJwt", Json.Encode.string x.tokensJwt )
        ]


type alias JWTData =
    { jwtUsername : String
    }


decodeJWTData : Decoder JWTData
decodeJWTData =
    decode JWTData
        |> required "jwtUsername" string


encodeJWTData : JWTData -> Json.Encode.Value
encodeJWTData x =
    Json.Encode.object
        [ ( "jwtUsername", Json.Encode.string x.jwtUsername )
        ]


type Gender
    = Male
    | Female


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\x ->
                case x of
                    "Male" ->
                        decode Male

                    "Female" ->
                        decode Female

                    _ ->
                        fail "Constructor not matched"
            )


encodeGender : Gender -> Json.Encode.Value
encodeGender x =
    case x of
        Male ->
            Json.Encode.string "Male"

        Female ->
            Json.Encode.string "Female"


type alias Profile =
    { profileUsername : String
    , profileEmail : String
    , profileAvatar : String
    , profileFirstName : Maybe String
    , profileSecondName : Maybe String
    , profileBirthday : Maybe Date
    , profileGender : Maybe Gender
    }


decodeProfile : Decoder Profile
decodeProfile =
    decode Profile
        |> required "profileUsername" string
        |> required "profileEmail" string
        |> required "profileAvatar" string
        |> required "profileFirstName" (nullable string)
        |> required "profileSecondName" (nullable string)
        |> required "profileBirthday" (nullable decodeDate)
        |> required "profileGender" (nullable decodeGender)


encodeProfile : Profile -> Json.Encode.Value
encodeProfile x =
    Json.Encode.object
        [ ( "profileUsername", Json.Encode.string x.profileUsername )
        , ( "profileEmail", Json.Encode.string x.profileEmail )
        , ( "profileAvatar", Json.Encode.string x.profileAvatar )
        , ( "profileFirstName", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.profileFirstName )
        , ( "profileSecondName", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.profileSecondName )
        , ( "profileBirthday", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << toUtcIsoString)) x.profileBirthday )
        , ( "profileGender", (Maybe.withDefault Json.Encode.null << Maybe.map encodeGender) x.profileGender )
        ]


type alias UserInfo =
    { infoFirstName : Maybe String
    , infoSecondName : Maybe String
    , infoBirthday : Maybe Date
    , infoGender : Maybe Gender
    }


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    decode UserInfo
        |> required "infoFirstName" (nullable string)
        |> required "infoSecondName" (nullable string)
        |> required "infoBirthday" (nullable decodeDate)
        |> required "infoGender" (nullable decodeGender)


encodeUserInfo : UserInfo -> Json.Encode.Value
encodeUserInfo x =
    Json.Encode.object
        [ ( "infoFirstName", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.infoFirstName )
        , ( "infoSecondName", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.infoSecondName )
        , ( "infoBirthday", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << toUtcIsoString)) x.infoBirthday )
        , ( "infoGender", (Maybe.withDefault Json.Encode.null << Maybe.map encodeGender) x.infoGender )
        ]


postUserLogin : Login -> Http.Request Tokens
postUserLogin body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "login"
                ]
        , body =
            Http.jsonBody (encodeLogin body)
        , expect =
            Http.expectJson decodeTokens
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserRegister : UserRegister -> Http.Request Tokens
postUserRegister body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "register"
                ]
        , body =
            Http.jsonBody (encodeUserRegister body)
        , expect =
            Http.expectJson decodeTokens
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserUsername : Http.Request String
postUserUsername =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "username"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserProfile : Http.Request Profile
postUserProfile =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "profile"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeProfile
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserEditUsername : String -> Http.Request NoContent
postUserEditUsername body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "edit"
                , "username"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserEditPassword : String -> Http.Request NoContent
postUserEditPassword body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "edit"
                , "password"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserEditEmail : String -> Http.Request NoContent
postUserEditEmail body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "edit"
                , "email"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserEditAvatar : Http.Request String
postUserEditAvatar =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "edit"
                , "avatar"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postUserEditProfile : UserInfo -> Http.Request NoContent
postUserEditProfile body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "edit"
                , "profile"
                ]
        , body =
            Http.jsonBody (encodeUserInfo body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
