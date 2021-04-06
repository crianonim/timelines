module Notes.ListNotes exposing (..)

import Html exposing (Attribute, Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Notes.Note as Note exposing (Note)


type alias Model =
    { notes : List Note
    }


type Msg
    = GetNotes
    | GotNotes (Result Http.Error (List Note))


init : ( Model, Cmd Msg )
init =
    ( { notes = []
      }
    , Note.getNotes GotNotes
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNotes ->
            ( model, Note.getNotes GotNotes )

        GotNotes (Ok notes) ->
            ( { model | notes = notes }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1
            []
            [ text "List Notes" ]
        , div []
            (List.map
                viewNote
                model.notes
            )
        , button [ onClick GetNotes ] [ text "Get Notes" ]
        ]


viewNote : Note -> Html Msg
viewNote n =
    div [] [ text (String.fromInt n.id), text " - ", div [] [ a [ href <| "notes/"  ++ String.fromInt n.id ] [ text n.content ] ] ]
