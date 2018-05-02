module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type NoContent = NoContent

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
    , profileFirstName : Maybe (String)
    , profileSecondName : Maybe (String)
    , profileBirthday : Maybe (Date)
    , profileGender : Maybe (Gender)
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

List (Profile)

(list decodeProfile)

(Json.Encode.list << List.map encodeProfile)

postUserLogin : Login -> Http.Request (Tokens)
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

postUserNew : UserRegister -> Http.Request (Tokens)
postUserNew body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "new"
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

postUserUsername : Http.Request (String)
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

postUserProfile : Http.Request (Profile)
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

postUserList : Http.Request (List (Profile))
postUserList =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "user"
                , "list"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeProfile)
        , timeout =
            Nothing
        , withCredentials =
            False
        }