module Data.User exposing (..)

import Data.Tokens exposing (Tokens, decodeTokens, encodeTokens)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias User =
    { tokens : Tokens
    }


decodeUser : Decode.Decoder User
decodeUser =
    decode User
        |> required "tokens" decodeTokens


encodeUser : User -> Encode.Value
encodeUser x =
    Encode.object
        [ ( "tokens", encodeTokens x.tokens )
        ]
