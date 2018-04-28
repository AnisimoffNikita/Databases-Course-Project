module Data.Tokens exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode

type alias Tokens =
    { tokensJwt : String
    }

decodeTokens : Decode.Decoder Tokens
decodeTokens =
    decode Tokens
        |> required "tokensJwt" Decode.string

encodeTokens : Tokens -> Encode.Value
encodeTokens x =
    Encode.object
        [ ( "tokensJwt", Encode.string x.tokensJwt )
        ]