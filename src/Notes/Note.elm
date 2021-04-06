module Notes.Note exposing (..)

import Http
import Json.Decode as Json
import Result


type alias Note =
    { id : Int
    , content : String
    }


api =
    "http://localhost:3000"


getNotes : (Result Http.Error (List Note) -> msg) -> Cmd msg
getNotes msg =
    Http.get
        { url = api ++ "/notes"
        , expect = Http.expectJson msg decodeNotes
        }


getNote : (Result Http.Error Note -> msg) -> Int -> Cmd msg
getNote msg id =
    Http.get
        { url = api ++ "/notes/" ++ String.fromInt id
        , expect = Http.expectJson msg decodeNote
        }


decodeNotes : Json.Decoder (List Note)
decodeNotes =
    Json.list decodeNote


decodeNote : Json.Decoder Note
decodeNote =
    Json.map2 Note
        (Json.field "id" Json.int)
        (Json.field "content" Json.string)
