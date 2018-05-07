module Data.Profile exposing (..)

import Date exposing (Date)
import Date.Extra exposing (toUtcIsoString)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type Gender
    = Male
    | Female


decodeGender : Decode.Decoder Gender
decodeGender =
    Decode.string
        |> Decode.andThen
            (\x ->
                case x of
                    "Male" ->
                        decode Male

                    "Female" ->
                        decode Female

                    _ ->
                        Decode.fail "Constructor not matched"
            )


encodeGender : Gender -> Encode.Value
encodeGender x =
    case x of
        Male ->
            Encode.string "Male"

        Female ->
            Encode.string "Female"


type alias Profile =
    { username : String
    , email : String
    , avatar : String
    , firstName : Maybe String
    , secondName : Maybe String
    , birthday : Maybe Date
    , gender : Maybe Gender
    }


decodeProfile : Decode.Decoder Profile
decodeProfile =
    decode Profile
        |> required "username" Decode.string
        |> required "email" Decode.string
        |> required "avatar" Decode.string
        |> required "firstname" (Decode.nullable Decode.string)
        |> required "secondname" (Decode.nullable Decode.string)
        |> required "birthday" (Decode.nullable Decode.date)
        |> required "gender" (Decode.nullable decodeGender)


encodeProfile : Profile -> Encode.Value
encodeProfile x =
    Encode.object
        [ ( "username", Encode.string x.username )
        , ( "email", Encode.string x.email )
        , ( "avatar", Encode.string x.avatar )
        , ( "firstName", (Maybe.withDefault Encode.null << Maybe.map Encode.string) x.firstName )
        , ( "secondName", (Maybe.withDefault Encode.null << Maybe.map Encode.string) x.secondName )
        , ( "birthday", (Maybe.withDefault Encode.null << Maybe.map (Encode.string << toUtcIsoString)) x.birthday )
        , ( "gender", (Maybe.withDefault Encode.null << Maybe.map encodeGender) x.gender )
        ]
