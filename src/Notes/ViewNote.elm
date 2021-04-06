module Notes.ViewNote exposing (..)

import Html exposing (Attribute, Html, div, text)
import Http
import Notes.Note as Note exposing (Note)
import Result


type alias Model =
    { note : Maybe Note
    }


type Msg
    = GotNote (Result Http.Error Note)


init : Int -> ( Model, Cmd Msg )
init id =
    ( { note = Nothing }, Note.getNote GotNote id )


update : Msg -> Model -> Model
update msg model =
    let
        _ =
            Debug.log "ms" msg
    in
    case msg of
        GotNote (Ok n) ->
            { model | note = Just n }

        _ ->
            model


view : Model -> Html Msg
view { note } =
    case note of
        Just n ->
            div [] [ text (String.fromInt n.id), text " - ", text n.content ]

        Nothing ->
            div [] [ text "Loading" ]
