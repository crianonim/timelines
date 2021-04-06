module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes exposing (href)
import Notes.Notes as Notes
import Platform.Cmd exposing (Cmd)
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
    }


type Msg
    = NotesMsg Notes.Msg
    | UrlRequest Browser.UrlRequest
    | ChangedUrl Url


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        route =
            Parser.parse urlParser url

        _ =
            Debug.log "ROUTE" route
    in
    routeToPage { page = TitlePage, navKey = navKey } route


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
                    ( model, Cmd.none )

        ChangedUrl url ->
            Parser.parse urlParser url |> routeToPage model



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
                        ]

                BadPage ->
                    text "BAD PAGE"
    }
