module Notes.Notes exposing (..)
import Html exposing (Attribute, Html, text)
import Notes.ListNotes as ListNotes
import Platform.Cmd exposing (Cmd)
import Url.Parser as Parser exposing ((</>))
import Notes.ViewNote as ViewNote

type Page
    = ListNotes ListNotes.Model
    | ViewNote ViewNote.Model
    | BadPage


type Route
    = ListNotesRoute
    | ViewNoteRoute Int


type alias Model =
    { page : Page
    }


type Msg
    = ListNotesMsg ListNotes.Msg
    | ViewNoteMsg ViewNote.Msg


init = { page = BadPage }

urlParser : Parser.Parser (Route -> c) c
urlParser =
    Parser.oneOf
        [ Parser.map ListNotesRoute Parser.top
        , Parser.map
            (ViewNoteRoute << Maybe.withDefault 1 << String.toInt)
            (Parser.string)
        ]

routeToPage : Model -> Maybe Route ->  ( Model, Cmd Msg )
routeToPage  model route=
    case route of
        Just ListNotesRoute ->
            let
                ( mod, fx ) =
                    ListNotes.init
            in
            ( { model | page = ListNotes mod }, fx |> Cmd.map ListNotesMsg )

        Just (ViewNoteRoute n) ->
            let
                ( mod, fx ) =
                    ViewNote.init n
            in
            ( { model | page = ViewNote mod }, fx |> Cmd.map ViewNoteMsg )

        Nothing ->
            ( { model | page = BadPage }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ListNotesMsg m ->
            case model.page of
                ListNotes notes ->
                    let
                        ( mod, fx ) =
                            ListNotes.update m notes
                    in
                    ( { model | page = ListNotes mod }, fx |> Cmd.map ListNotesMsg )

                _ ->
                    ( model, Cmd.none )

        ViewNoteMsg m ->
            case model.page of
                ViewNote single ->
                    let
                        mod =
                            ViewNote.update m single
                    in
                    ( { model | page = ViewNote mod }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =

            case model.page of
                ListNotes notes ->
                    ListNotes.view notes |> Html.map ListNotesMsg

                ViewNote m ->
                    ViewNote.view m |> Html.map ViewNoteMsg

                BadPage ->
                    text "BAD PAGE"
