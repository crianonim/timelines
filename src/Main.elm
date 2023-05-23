module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Date
import Html exposing (Attribute, Html, a, div, h1, input, text)
import Html.Attributes as Attrs exposing (href, style, type_, value)
import Html.Events exposing (onInput)
import Notes.Notes as Notes
import Platform.Cmd exposing (Cmd)
import Timeline.Timeline as Timeline exposing (Timeline)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , onUrlChange = ChangedUrl
        , onUrlRequest = UrlRequest
        , subscriptions = subscriptions
        , view = view
        }


type Page
    = NotesPage Notes.Model
    | TimelinesPage Timeline.Model
    | BadPage


type Route
    = TitleRoute
    | NotesRoute Notes.Route


type alias Model =
    { page : Page
    , navKey : Nav.Key
    }


type Msg
    = NotesMsg Notes.Msg
    | TimelienMsg Timeline.Msg
    | UrlRequest Browser.UrlRequest
    | ChangedUrl Url


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        route =
            Parser.parse urlParser url

        ( tlModel, _ ) =
            Timeline.init
    in
    routeToPage { page = TimelinesPage tlModel, navKey = navKey } route


urlParser : Parser.Parser (Route -> c) c
urlParser =
    Parser.oneOf
        [ Parser.map TitleRoute Parser.top
        , Parser.map NotesRoute (Parser.s "notes" </> Notes.urlParser)
        ]


routeToPage : Model -> Maybe Route -> ( Model, Cmd Msg )
routeToPage model route =
    case route of
        Just (NotesRoute notesRoute) ->
            let
                ( mod, fx ) =
                    Notes.routeToPage Notes.init <| Just notesRoute
            in
            ( { model | page = NotesPage mod }, fx |> Cmd.map NotesMsg )

        Just TitleRoute ->
            let
                ( tlModel, effect ) =
                    Timeline.init
            in
            ( { model | page = TimelinesPage tlModel }, effect |> Cmd.map TimelienMsg )

        Nothing ->
            ( { model | page = BadPage }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NotesMsg notesMsg ->
            case model.page of
                NotesPage notesModel ->
                    let
                        ( mod, fx ) =
                            Notes.update notesMsg notesModel
                    in
                    ( { model | page = NotesPage mod }, fx |> Cmd.map NotesMsg )

                _ ->
                    ( model, Cmd.none )

        UrlRequest urlReq ->
            case urlReq of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External urlString ->
                    ( model, Nav.load urlString )

        ChangedUrl url ->
            Parser.parse urlParser url |> routeToPage model

        TimelienMsg tlMsg ->
            case model.page of
                TimelinesPage tlModel ->
                    let
                        ( mod, fx ) =
                            Timeline.update tlMsg tlModel
                    in
                    ( { model | page = TimelinesPage mod }, fx |> Cmd.map TimelienMsg )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Timelines"
    , body =
        List.singleton <|
            case model.page of
                NotesPage notesModel ->
                    Notes.view notesModel |> Html.map NotesMsg

                TimelinesPage timelinesModel ->
                    Timeline.view timelinesModel |> Html.map TimelienMsg

                BadPage ->
                    text "BAD PAGE"
    }
