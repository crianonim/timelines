module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, div, h1, input, text)
import Html.Attributes exposing (href, style, type_, value)
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
    | TitlePage
    | BadPage


type Route
    = TitleRoute
    | NotesRoute Notes.Route


type alias Model =
    { page : Page
    , navKey : Nav.Key
    , timelines : List Timeline
    , viewPort : Timeline.Viewport
    }


type Msg
    = NotesMsg Notes.Msg
    | UrlRequest Browser.UrlRequest
    | ChangedUrl Url
    | UpdateStart String
    | UpdateEnd String


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        route =
            Parser.parse urlParser url
    in
    routeToPage { page = TitlePage, navKey = navKey, timelines = Timeline.data, viewPort = Timeline.exampleVP } route


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
            ( { model | page = TitlePage }, Cmd.none )

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

        UpdateStart s ->
            let
                newValue =
                    case String.toInt s of
                        Just i ->
                            i

                        Nothing ->
                            if String.length s == 0 then
                                0

                            else
                                model.viewPort.start

                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | start = newValue } }, Cmd.none )

        UpdateEnd s ->
            let
                newValue =
                    case String.toInt s of
                        Just i ->
                            i

                        Nothing ->
                            if String.length s == 0 then
                                0

                            else
                                model.viewPort.end

                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | end = newValue } }, Cmd.none )



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

                TitlePage ->
                    div []
                        [ h1 []
                            [ text "Welcome to Timelines"
                            ]
                        , a [ href "notes" ] [ text "Notes" ]
                        , div []
                            (List.map
                                Timeline.viewTimeline
                                model.timelines
                            )
                        , input [ type_ "number", onInput UpdateStart, value <| String.fromInt model.viewPort.start ] []
                        , input [ type_ "number", onInput UpdateEnd, value <| String.fromInt model.viewPort.end ] []
                        , text "Visible"
                        , div []
                            (List.map
                                Timeline.viewTimeline
                                (List.filter (Timeline.isInViewport model.viewPort) model.timelines)
                            )
                        , div [] [ a [ href "https://github.com/crianonim/timelines" ] [ text "Github repo" ] ]

                        --, div [] (List.map (\( t, e ) -> Timeline.viewBar t e 500) Timeline.example)
                        ]

                BadPage ->
                    text "BAD PAGE"
    }
