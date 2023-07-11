module Notes.Note exposing (..)

import Http
import Json.Decode as Json
import Json.Encode
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

createNote : String -> (Result Http.Error Note -> msg)-> Cmd msg
createNote noteContent wrapMsg=
     Http.post
            { url = api ++ "/notes"
            , body = Http.jsonBody <| Json.Encode.object [("content", Json.Encode.string noteContent)]
            , expect = Http.expectJson wrapMsg decodeNote
            }

