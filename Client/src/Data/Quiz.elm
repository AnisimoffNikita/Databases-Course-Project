module Data.Quiz exposing (..)

import Date exposing (Date)
import Date.Extra exposing (toUtcIsoString)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode


type alias Question =
    { text : String
    , answer : String
    , variants : List (String)
    }

decodeQuestion : Decode.Decoder Question
decodeQuestion =
    decode Question
        |> required "text" Decode.string
        |> required "answer" Decode.string
        |> required "variants" (Decode.list Decode.string)

encodeQuestion : Question -> Encode.Value
encodeQuestion x =
    Encode.object
        [ ( "text", Encode.string x.text )
        , ( "answer", Encode.string x.answer )
        , ( "variants", (Encode.list << List.map Encode.string) x.variants )
        ]

type alias Quiz =
    { name : String
    , description : String
    , creationDate : Date
    , passingNumber : Int
    , questions : List Question
    }

decodeQuiz : Decode.Decoder Quiz
decodeQuiz =
    decode Quiz
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "creationdate" Decode.date
        |> required "passingnumber" Decode.int
        |> required "quizquestions" (Decode.list decodeQuestion)

encodeQuiz : Quiz -> Encode.Value
encodeQuiz x =
    Encode.object
        [ ( "name", Encode.string x.name )
        , ( "description", Encode.string x.description )
        , ( "creationDate", (Encode.string << toUtcIsoString) x.creationDate )
        , ( "passingNumber", Encode.int x.passingNumber )
        , ( "questions", (Encode.list << List.map encodeQuestion) x.questions )
        ]
