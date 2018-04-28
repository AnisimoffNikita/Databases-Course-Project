module Data.Session exposing (..)

import Data.User exposing (User, decodeUser, encodeUser)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode

type alias Session = 
    { user : Maybe User
    }


decodeSession : Decode.Decoder Session
decodeSession =
    decode Session
        |> required "user" (Decode.nullable decodeUser)



encodeSession : Session -> Encode.Value
encodeSession session =
    Encode.object
        [ ( "user", (Maybe.withDefault Encode.null << Maybe.map encodeUser) session.user )
        ]

decodeSessionFromJson : Decode.Value -> Maybe Session
decodeSessionFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString decodeSession >> Result.toMaybe)