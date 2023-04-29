module Notes.ListNotes exposing (..)

import Html exposing (Attribute, Html, a, button, div, h1, text)
import Html.Attributes as Attrs exposing (href)
import Html.Events as Events exposing (onClick)
import Http
import Notes.Note as Note exposing (Note)


type alias Model =
    { notes : List Note
    , content : String
    }


type Msg
    = GetNotes
    | GotNotes (Result Http.Error (List Note))
    | ContentInput String
    | CreateNote
    | NoteCreated (Result Http.Error Note)


init : ( Model, Cmd Msg )
init =
    ( { notes = []
      , content = ""
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

        ContentInput str ->
            ( { model | content = str }, Cmd.none )

        CreateNote ->
            ( model, Note.createNote model.content NoteCreated )

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
        , Html.input [ Attrs.value model.content, Events.onInput ContentInput ] []
        , Html.button [ Events.onClick CreateNote ] [ Html.text "Create Note" ]
        ]


viewNote : Note -> Html Msg
viewNote n =
    div []
        [ text (String.fromInt n.id)
        , text " - "
        , div [] [ a [ href <| "notes/" ++ String.fromInt n.id ] [ text n.content ] ]
        ]
