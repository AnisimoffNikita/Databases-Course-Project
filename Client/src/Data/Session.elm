module Data.Session exposing (..)

import Data.User exposing (User, decodeUser, encodeUser)
import Data.Tokens exposing (Tokens, decodeTokens, encodeTokens)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Debug exposing (..)

type alias Session = 
    { tokens : Maybe Tokens
    }


decodeSession : Decode.Decoder Session
decodeSession =
    decode Session
        |> required "tokens" (Decode.nullable decodeTokens)



encodeSession : Session -> Encode.Value
encodeSession session =
    Encode.object
        [ ( "tokens", (Maybe.withDefault Encode.null << Maybe.map encodeTokens) session.tokens )
        ]

decodeSessionFromJson : Decode.Value -> Maybe Session
decodeSessionFromJson json =
    let 
        res = Decode.decodeValue Decode.string json
        val = case res of 
            Ok val -> val
            Err val -> val
    in
    
    log ("!!!" ++ val) (json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString decodeSession >> Result.toMaybe))