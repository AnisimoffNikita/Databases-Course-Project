module Data.User exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Data.Tokens exposing (Tokens, decodeTokens, encodeTokens)

type alias User = 
    { username : String
    , avatar : String 
    , tokens : Tokens
    }

decodeUser : Decode.Decoder User
decodeUser =
    decode User
        |> required "username" Decode.string
        |> required "avatar" Decode.string
        |> required "tokens" decodeTokens

encodeUser : User -> Encode.Value
encodeUser x =
    Encode.object
        [ ( "username", Encode.string x.username )
        , ( "avatar", Encode.string x.avatar )
        , ( "tokens", encodeTokens x.tokens )
        ]