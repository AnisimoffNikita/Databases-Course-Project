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

type alias UserInfo =
    { infoFirstName : Maybe (String)
    , infoSecondName : Maybe (String)
    , infoBirthday : Maybe (Date)
    , infoGender : Maybe (Gender)
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

type QuestionOptions
    = CheckOption Int
    | RadioOption Int
    | LineOption

decodeQuestionOptions : Decoder QuestionOptions
decodeQuestionOptions =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "CheckOption" ->
                        decode CheckOption
                            |> required "contents" int

                    "RadioOption" ->
                        decode RadioOption
                            |> required "contents" int

                    "LineOption" ->
                        decode LineOption

                    _ ->
                        fail "Constructor not matched"
            )

encodeQuestionOptions : QuestionOptions -> Json.Encode.Value
encodeQuestionOptions x =
    case x of
        CheckOption y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CheckOption" )
                , ( "contents", Json.Encode.int y0 )
                ]

        RadioOption y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RadioOption" )
                , ( "contents", Json.Encode.int y0 )
                ]

        LineOption ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "LineOption" )
                , ( "contents", Json.Encode.list [] )
                ]

type QuestionAnswer
    = CheckAnswer Int
    | RadioAnswer Int
    | LineAnswer String

decodeQuestionAnswer : Decoder QuestionAnswer
decodeQuestionAnswer =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "CheckAnswer" ->
                        decode CheckAnswer
                            |> required "contents" int

                    "RadioAnswer" ->
                        decode RadioAnswer
                            |> required "contents" int

                    "LineAnswer" ->
                        decode LineAnswer
                            |> required "contents" string

                    _ ->
                        fail "Constructor not matched"
            )

encodeQuestionAnswer : QuestionAnswer -> Json.Encode.Value
encodeQuestionAnswer x =
    case x of
        CheckAnswer y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "CheckAnswer" )
                , ( "contents", Json.Encode.int y0 )
                ]

        RadioAnswer y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "RadioAnswer" )
                , ( "contents", Json.Encode.int y0 )
                ]

        LineAnswer y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "LineAnswer" )
                , ( "contents", Json.Encode.string y0 )
                ]

type alias Question =
    { questionText : String
    , questionAnswer : String
    , questionVariants : List (String)
    }

decodeQuestion : Decoder Question
decodeQuestion =
    decode Question
        |> required "questionText" string
        |> required "questionAnswer" string
        |> required "questionVariants" (list string)

encodeQuestion : Question -> Json.Encode.Value
encodeQuestion x =
    Json.Encode.object
        [ ( "questionText", Json.Encode.string x.questionText )
        , ( "questionAnswer", Json.Encode.string x.questionAnswer )
        , ( "questionVariants", (Json.Encode.list << List.map Json.Encode.string) x.questionVariants )
        ]

type alias Quiz =
    { quizName : String
    , quizDescription : String
    , quizCreationDate : Date
    , quizPassingNumber : Int
    , quizQuestions : List (Question)
    }

decodeQuiz : Decoder Quiz
decodeQuiz =
    decode Quiz
        |> required "quizName" string
        |> required "quizDescription" string
        |> required "quizCreationDate" decodeDate
        |> required "quizPassingNumber" int
        |> required "quizQuestions" (list decodeQuestion)

encodeQuiz : Quiz -> Json.Encode.Value
encodeQuiz x =
    Json.Encode.object
        [ ( "quizName", Json.Encode.string x.quizName )
        , ( "quizDescription", Json.Encode.string x.quizDescription )
        , ( "quizCreationDate", (Json.Encode.string << toUtcIsoString) x.quizCreationDate )
        , ( "quizPassingNumber", Json.Encode.int x.quizPassingNumber )
        , ( "quizQuestions", (Json.Encode.list << List.map encodeQuestion) x.quizQuestions )
        ]

type alias QuizPreview =
    { name : String
    , description : String
    , passingNumber : Int
    , id : String
    }

decodeQuizPreview : Decoder QuizPreview
decodeQuizPreview =
    decode QuizPreview
        |> required "name" string
        |> required "description" string
        |> required "passingNumber" int
        |> required "id" string

encodeQuizPreview : QuizPreview -> Json.Encode.Value
encodeQuizPreview x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        , ( "description", Json.Encode.string x.description )
        , ( "passingNumber", Json.Encode.int x.passingNumber )
        , ( "id", Json.Encode.string x.id )
        ]

type alias QuestionWithoutAnswer =
    { text : String
    , variants : List (String)
    }

decodeQuestionWithoutAnswer : Decoder QuestionWithoutAnswer
decodeQuestionWithoutAnswer =
    decode QuestionWithoutAnswer
        |> required "text" string
        |> required "variants" (list string)

encodeQuestionWithoutAnswer : QuestionWithoutAnswer -> Json.Encode.Value
encodeQuestionWithoutAnswer x =
    Json.Encode.object
        [ ( "text", Json.Encode.string x.text )
        , ( "variants", (Json.Encode.list << List.map Json.Encode.string) x.variants )
        ]

type alias QuizWithoutAnswers =
    { dataName : String
    , dataDescription : String
    , dataQuestions : List (QuestionWithoutAnswer)
    }

decodeQuizWithoutAnswers : Decoder QuizWithoutAnswers
decodeQuizWithoutAnswers =
    decode QuizWithoutAnswers
        |> required "dataName" string
        |> required "dataDescription" string
        |> required "dataQuestions" (list decodeQuestionWithoutAnswer)

encodeQuizWithoutAnswers : QuizWithoutAnswers -> Json.Encode.Value
encodeQuizWithoutAnswers x =
    Json.Encode.object
        [ ( "dataName", Json.Encode.string x.dataName )
        , ( "dataDescription", Json.Encode.string x.dataDescription )
        , ( "dataQuestions", (Json.Encode.list << List.map encodeQuestionWithoutAnswer) x.dataQuestions )
        ]

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

postUserRegister : UserRegister -> Http.Request (Tokens)
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

postUserEditUsername : String -> Http.Request (Tokens)
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
            Http.expectJson decodeTokens
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postUserEditPassword : String -> Http.Request (NoContent)
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

postUserEditEmail : String -> Http.Request (NoContent)
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

postUserEditAvatar : Http.Request (String)
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

postUserEditProfile : UserInfo -> Http.Request (NoContent)
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

postQuizNew : Quiz -> Http.Request (NoContent)
postQuizNew body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "quiz"
                , "new"
                ]
        , body =
            Http.jsonBody (encodeQuiz body)
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

postQuizGet : Http.Request (List (QuizPreview))
postQuizGet =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "quiz"
                , "get"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeQuizPreview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postQuizGetAll : Http.Request (List (QuizPreview))
postQuizGetAll =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "quiz"
                , "get"
                , "all"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeQuizPreview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postQuizRemove : String -> Http.Request (NoContent)
postQuizRemove body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "quiz"
                , "remove"
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

postQuizGetById : String -> Http.Request (QuizWithoutAnswers)
postQuizGetById capture_id =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "quiz"
                , "get"
                , capture_id |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeQuizWithoutAnswers
        , timeout =
            Nothing
        , withCredentials =
            False
        }