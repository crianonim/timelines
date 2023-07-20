module Timeline.Timeline exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, a, div, h1, h2, text)
import Html.Attributes as Attrs exposing (href, style, title)
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode
import Time
import Timeline.API exposing (Period(..), PeriodType(..), TimePoint(..), Timeline, Viewport)


type alias Model =
    { timelines : List Timeline
    , viewPort : Viewport
    , newTimelinePeriod : Period
    , newTimelineName : String
    , isEditingId : Maybe Int
    }


type Msg
    = UpdateStart TimePoint
    | UpdateEnd TimePoint
    | GotTimelines (Result Http.Error (List Timeline))
    | UpdatePeriod Period
    | UpdateNewTimelineName String
    | SaveTimeline (Maybe Int) Period String
    | SavedTimeline (Maybe Int) (Result Http.Error Timeline)
    | RemoveTimeline Int
    | RemovedTimeline Int (Result Http.Error ())
    | EditTimeline Int
    | AllPeriods
    | SetPeriod Viewport
    | Sort


init : ( Model, Cmd Msg )
init =
    ( { timelines = [], viewPort = exampleVP, newTimelinePeriod = Point <| Year 2023, newTimelineName = "", isEditingId = Nothing }, getTimelines GotTimelines )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStart s ->
            let
                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | start = Timeline.API.timepointMin s vp.end } }, Cmd.none )

        UpdateEnd s ->
            let
                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | end = Timeline.API.timepointMax s vp.start } }, Cmd.none )

        SaveTimeline mId period name ->
            ( model
            , case mId of
                Nothing ->
                    saveNewTimeline { id = 0, name = name, period = period } (SavedTimeline mId)

                Just id ->
                    saveEditTimeline { id = id, name = name, period = period } (SavedTimeline mId)
            )

        SavedTimeline mId (Ok tl) ->
            ( { model
                | timelines =
                    case mId of
                        Just id ->
                            model.timelines
                                |> List.map
                                    (\t ->
                                        if t.id == id then
                                            tl

                                        else
                                            t
                                    )

                        Nothing ->
                            model.timelines ++ [ tl ]
                , isEditingId = Nothing
                , newTimelineName = ""
              }
            , Cmd.none
            )

        SavedTimeline _ (Err err) ->
            let
                _ =
                    Debug.log "error saving " err
            in
            ( model, Cmd.none )

        GotTimelines result ->
            case result of
                Ok timelines ->
                    ( { model | timelines = timelines }, Cmd.none )

                Err e ->
                    let
                        _ =
                            Debug.log "Error " e
                    in
                    ( model, Cmd.none )

        UpdateNewTimelineName name ->
            ( { model | newTimelineName = name }, Cmd.none )

        UpdatePeriod period ->
            ( { model | newTimelinePeriod = period }, Cmd.none )

        RemoveTimeline id ->
            ( model, removeTimeline id (RemovedTimeline id) )

        RemovedTimeline id (Ok ()) ->
            ( { model | timelines = List.filter (\tl -> tl.id /= id) model.timelines }, Cmd.none )

        RemovedTimeline id (Err e) ->
            let
                _ =
                    Debug.log "remove error" ( id, e )
            in
            ( model, Cmd.none )

        EditTimeline id ->
            let
                tl : Maybe Timeline
                tl =
                    model.timelines
                        |> List.filter (\t -> t.id == id)
                        |> List.head
            in
            ( case tl of
                Nothing ->
                    model

                Just timeline ->
                    { model
                        | isEditingId = Just id
                        , newTimelineName = timeline.name
                        , newTimelinePeriod = timeline.period
                    }
            , Cmd.none
            )

        AllPeriods ->
            let
                viewPort =
                    { start =
                        List.map .period model.timelines
                            |> Timeline.API.startOfListOfPeriods
                            |> Maybe.withDefault (Year 2000)
                    , end =
                        List.map .period model.timelines
                            |> Timeline.API.endOfListOfPeriods
                            |> Maybe.withDefault (Year 2020)
                    }
            in
            ( { model | viewPort = viewPort }
            , Cmd.none
            )

        SetPeriod viewPort ->
            ( { model | viewPort = viewPort }
            , Cmd.none
            )

        Sort ->
            ( { model
                | timelines =
                    List.sortWith
                        (\a b ->
                            case
                                ( Timeline.API.minTimePointOfPeriod a.period
                                , Timeline.API.minTimePointOfPeriod b.period
                                )
                            of
                                ( _, Nothing ) ->
                                    GT

                                ( Nothing, _ ) ->
                                    LT

                                ( Just x, Just y ) ->
                                    Date.compare (Timeline.API.timePointToStartDate x) (Timeline.API.timePointToStartDate y)
                        )
                        model.timelines
              }
            , Cmd.none
            )


type alias TimeLineBar =
    { start : Maybe Float
    , length : Float
    , timeline : Timeline
    }


data =
    [ Timeline 0 (Point <| YearMonthDay 1980 Time.Jun 6) "Jan's Birthday"
    , Timeline 1 (Closed (Year 1995) (Year 1999)) "High School"
    , Timeline 2 (Started (Year 2019)) "Working in Permutive"
    , Timeline 3 (Closed (Year 1952) (Year 2022)) "Queen Elizabeth II reign"
    , Timeline 4 (Started (Year 2005)) "Moved to the UK"
    , Timeline 5 (Closed (Year 1789) (Year 1795)) "French Revolution"
    , Timeline 6 (Closed (Year 2014) (Year 2018)) "Lived in Clapham"
    , Timeline 7 (Finished (Year 1985)) "Finished in 1985"
    , Timeline 8 (Point <| YearMonthDay 2023 Time.Apr 6) "Left Permutive"
    , Timeline 9 (Point <| Year 2022) "Bad Year"
    , Timeline 10 (Point <| YearMonth 2023 Time.Jun) "Pride Month"
    ]


isInViewport : Viewport -> Period -> Bool
isInViewport { start, end } period =
    let
        viewportStart =
            Timeline.API.timePointToStartDate start

        viewportEnd =
            Timeline.API.timePointToEndDate end
    in
    case period of
        Point tp ->
            let
                periodStart =
                    Timeline.API.timePointToStartDate tp

                periodEnd =
                    Timeline.API.timePointToEndDate tp
            in
            (Date.compare periodStart viewportStart == GT || Date.compare periodStart viewportStart == EQ)
                && (Date.compare periodEnd viewportEnd == LT || Date.compare periodEnd viewportEnd == EQ)

        Closed startTP endTP ->
            let
                periodStart =
                    Timeline.API.timePointToStartDate startTP

                periodEnd =
                    Timeline.API.timePointToEndDate endTP
            in
            --((Date.compare periodEnd viewportStart == GT || Date.compare periodEnd viewportStart == EQ)
            --    && (Date.compare periodEnd viewportEnd == LT || Date.compare periodEnd viewportEnd == EQ)
            --)
            --    || ((Date.compare periodStart viewportStart == GT || Date.compare periodStart viewportStart == EQ)
            --            && (Date.compare periodStart viewportEnd == LT || Date.compare periodStart viewportEnd == EQ)
            --       )
            (Date.compare periodStart viewportEnd == LT || Date.compare periodStart viewportEnd == EQ) && (Date.compare periodEnd viewportStart == GT || Date.compare periodEnd viewportStart == EQ)

        --startYear >= start && startYear <= end || endYear >= start && endYear <= end
        Started tp ->
            let
                periodStart =
                    Timeline.API.timePointToStartDate tp
            in
            Date.compare periodStart viewportEnd == LT || Date.compare periodStart viewportEnd == EQ

        Finished tp ->
            let
                periodStart =
                    Timeline.API.timePointToStartDate tp
            in
            Date.compare periodStart viewportStart == GT || Date.compare periodStart viewportStart == EQ


isPeriodFinished : Period -> Bool
isPeriodFinished period =
    case period of
        Finished _ ->
            True

        _ ->
            False


timelineToTimelineBar : Viewport -> Float -> Timeline -> TimeLineBar
timelineToTimelineBar { start, end } width ({ period, name } as tl) =
    let
        viewportStart =
            Timeline.API.timePointToStartDate start

        viewportEnd =
            Timeline.API.timePointToEndDate end

        ( dateStart, dateEnd ) =
            case period of
                Point tp ->
                    ( Timeline.API.timePointToStartDate tp, Timeline.API.timePointToEndDate tp )

                Closed y1 y2 ->
                    ( Timeline.API.timePointToStartDate y1, Date.min (Timeline.API.timePointToEndDate y2) viewportEnd )

                Started tp ->
                    ( Timeline.API.timePointToStartDate tp, viewportEnd )

                Finished tp ->
                    ( viewportStart, Date.min (Timeline.API.timePointToEndDate tp) viewportEnd )

        viewPortDays =
            Date.diff Date.Days viewportStart viewportEnd

        scale =
            width
                / toFloat viewPortDays
    in
    { start =
        if Date.compare viewportStart dateStart == GT || isPeriodFinished period then
            Nothing

        else
            Just (toFloat (Date.diff Date.Days viewportStart dateStart) * scale)
    , length = toFloat (Date.diff Date.Days (Date.max viewportStart dateStart) dateEnd) * scale
    , timeline = tl
    }


exampleVP : Viewport
exampleVP =
    { start = YearMonthDay 2020 Time.Mar 13
    , end = Year 2023
    }


viewTimeline : Timeline -> Html Msg
viewTimeline tl =
    div []
        [ text <| Timeline.API.toString tl
        , Html.button [ Events.onClick <| RemoveTimeline tl.id ] [ Html.text " [x]" ]
        , Html.button [ Events.onClick <| EditTimeline tl.id ] [ Html.text " [edit]" ]
        , case tl.period of
            Closed p1 p2 ->
                Html.button [ Events.onClick <| SetPeriod (Viewport p1 p2) ] [ Html.text " [set viewport]" ]

            _ ->
                Html.text ""
        ]


viewBar : TimeLineBar -> Html msg
viewBar { timeline, start, length } =
    let
        endStyle =
            case timeline.period of
                Started _ ->
                    style "border-right-style" "dashed"

                _ ->
                    style "border-right-style" "solid"

        startStyle =
            case start of
                Just _ ->
                    style "border-left-style" "solid"

                Nothing ->
                    style "border-left-style" "dashed"

        startPoint =
            Maybe.withDefault 0 start
    in
    div
        [ style "margin-left" (String.fromFloat startPoint ++ "px")
        , style "width" (String.fromFloat length ++ "px")
        , Attrs.class "bg-sky-500 h-4 border border-black"
        , endStyle
        , startStyle
        , title timeline.name
        ]
        []


api =
    "http://localhost:3000"


getTimelines : (Result Http.Error (List Timeline) -> msg) -> Cmd msg
getTimelines wrapMsg =
    Http.get
        { url = api ++ "/timelines"
        , expect =
            Http.expectJson wrapMsg <|
                Json.Decode.list decodeTimeline
        }


saveNewTimeline : Timeline -> (Result Http.Error Timeline -> msg) -> Cmd msg
saveNewTimeline timeline wrapMsg =
    Http.post
        { url = api ++ "/timelines"
        , body = Http.jsonBody <| Json.Encode.object [ ( "timeline", encodeTimeline timeline ) ]
        , expect = Http.expectJson wrapMsg decodeTimeline
        }


saveEditTimeline : Timeline -> (Result Http.Error Timeline -> msg) -> Cmd msg
saveEditTimeline timeline wrapMsg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = api ++ "/timelines/" ++ String.fromInt timeline.id
        , body = Http.jsonBody <| Json.Encode.object [ ( "timeline", encodeTimeline timeline ) ]
        , expect = Http.expectJson wrapMsg decodeTimeline
        , timeout = Nothing
        , tracker = Nothing
        }


removeTimeline : Int -> (Result Http.Error () -> msg) -> Cmd msg
removeTimeline id wrapMsg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = api ++ "/timelines/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever wrapMsg
        , timeout = Nothing
        , tracker = Nothing
        }


encodePeriod : Period -> Json.Encode.Value
encodePeriod period =
    Timeline.API.periodToString period |> Json.Encode.string


encodeTimeline : Timeline -> Json.Encode.Value
encodeTimeline timeline =
    Json.Encode.object <|
        [ ( "period", encodePeriod timeline.period )
        , ( "name", Json.Encode.string timeline.name )
        ]


stringToTimePoint : String -> Maybe TimePoint
stringToTimePoint string =
    case String.split "-" string of
        [ yearStr, monthStr, dayStr ] ->
            Maybe.map3 (\year month day -> YearMonthDay year month day)
                (String.toInt yearStr)
                (String.toInt monthStr |> Maybe.map Date.numberToMonth)
                (String.toInt dayStr)

        [ yearStr, monthStr ] ->
            Maybe.map2 (\year month -> YearMonth year month)
                (String.toInt yearStr)
                (String.toInt monthStr |> Maybe.map Date.numberToMonth)

        [ yearStr ] ->
            Maybe.map Year (String.toInt yearStr)

        _ ->
            Nothing


decodePeriod : Json.Decode.Decoder Period
decodePeriod =
    let
        get id =
            case String.split " - " id of
                [ point ] ->
                    stringToTimePoint point
                        |> Maybe.map (Point >> Json.Decode.succeed)
                        |> Maybe.withDefault (Json.Decode.fail "")

                [ start, "" ] ->
                    stringToTimePoint start
                        |> Maybe.map (Started >> Json.Decode.succeed)
                        |> Maybe.withDefault (Json.Decode.fail "")

                [ "", end ] ->
                    stringToTimePoint end
                        |> Maybe.map (Finished >> Json.Decode.succeed)
                        |> Maybe.withDefault (Json.Decode.fail "")

                [ start, end ] ->
                    Maybe.map2 (\s e -> Closed s e |> Json.Decode.succeed)
                        (stringToTimePoint start)
                        (stringToTimePoint end)
                        |> Maybe.withDefault (Json.Decode.fail "")

                _ ->
                    Json.Decode.fail ("unknown value for Period: " ++ id)
    in
    Json.Decode.string |> Json.Decode.andThen get


decodeTimeline : Json.Decode.Decoder Timeline
decodeTimeline =
    Json.Decode.map3 Timeline
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.at [ "timeline", "period" ] decodePeriod)
        (Json.Decode.at [ "timeline", "name" ] Json.Decode.string)


view : Model -> Html Msg
view model =
    div []
        [ h1 [ Attrs.class "text-xl mb-4" ]
            [ text "Welcome to Timelines"
            ]
        , div [] [ viewNewTimeline model.newTimelinePeriod model.newTimelineName model.isEditingId ]
        , div [ Attrs.class "flex gap-4" ]
            [ Html.button [ Events.onClick AllPeriods ] [ Html.text "ALL" ]
            , Html.button [ Events.onClick Sort ] [ Html.text "Sort" ]
            , div [] [ text "From:" ]
            , viewTimepointSelector { onSelected = UpdateStart, timepoint = model.viewPort.start }
            , div [] [ text "To:" ]
            , viewTimepointSelector { onSelected = UpdateEnd, timepoint = model.viewPort.end }
            ]
        , div
            [ Attrs.class "border border-slate-500  m-2"
            ]
            (List.map
                (\tl ->
                    let
                        bar =
                            timelineToTimelineBar model.viewPort 500 tl
                    in
                    Html.div [ Attrs.class "flex gap-4 items-center" ]
                        [ Html.div [ Attrs.class "w-[500px]" ] [ viewBar bar ]
                        , Html.div [ Attrs.class "text-sm" ] [ viewTimeline tl ]
                        ]
                )
                (List.filter (.period >> isInViewport model.viewPort) model.timelines)
            )

        --, h2 [] [ text "> Visible" ]
        --, div []
        --    (List.map
        --        viewTimeline
        --        (List.filter (.period >> isInViewport model.viewPort) model.timelines)
        --    )
        , div [ Attrs.class "border border-slate-500 h-[240px] overflow-scroll" ]
            [ h2 [] [ text "> All entries" ]
            , div []
                (List.map
                    viewTimeline
                    model.timelines
                )
            ]
        , a [ href "notes" ] [ text "Notes" ]
        , div [] [ a [ href "https://github.com/crianonim/timelines" ] [ text "Github repo" ] ]
        ]


allMonths =
    List.range 1 12 |> List.map Date.numberToMonth


type alias TimepointSelectorConfig msg =
    { timepoint : TimePoint
    , onSelected : TimePoint -> msg
    }


daysInAMonth : Int -> Date.Month -> Int
daysInAMonth year month =
    Date.fromCalendarDate year month 2
        |> Date.ceiling Date.Month
        |> Date.add Date.Days -1
        |> Date.day


viewTimepointSelector : TimepointSelectorConfig msg -> Html msg
viewTimepointSelector config =
    let
        maxDays =
            case config.timepoint of
                Year _ ->
                    Nothing

                YearMonth year month ->
                    Just <| daysInAMonth year month

                YearMonthDay year month _ ->
                    Just <| daysInAMonth year month

        emptyValue =
            "--"

        parseYear str =
            String.toInt str |> Maybe.withDefault 0

        getMonth : TimePoint -> Maybe String
        getMonth tp =
            case tp of
                Year _ ->
                    Nothing

                YearMonth _ month ->
                    Just <| Timeline.API.monthToNumeral month

                YearMonthDay _ month _ ->
                    Just <| Timeline.API.monthToNumeral month

        monthListener : String -> TimePoint
        monthListener month =
            String.toInt month
                |> Maybe.map Date.numberToMonth
                |> Maybe.map (\m -> YearMonth (Timeline.API.timepointToYear config.timepoint) m)
                |> Maybe.withDefault (Year (Timeline.API.timepointToYear config.timepoint))

        dayListener : Int -> Time.Month -> String -> TimePoint
        dayListener year month dayString =
            String.toInt dayString
                |> Maybe.map (\d -> YearMonthDay year month d)
                |> Maybe.withDefault (YearMonth year month)

        daysOption currentDay days =
            Html.option [] [ Html.text emptyValue ]
                :: (List.range 1 days
                        |> List.map (\m -> Html.option [ Attrs.selected (m == currentDay) ] [ Html.text <| String.fromInt m ])
                   )
    in
    Html.div []
        [ Html.input
            [ Attrs.type_ "number"
            , Attrs.class "w-16"
            , Events.onInput (parseYear >> Year >> config.onSelected)
            , Attrs.value <| String.fromInt <| Timeline.API.timepointToYear config.timepoint
            ]
            []
        , Html.select
            [ Attrs.value <| Maybe.withDefault emptyValue <| getMonth config.timepoint
            , Events.onInput (monthListener >> config.onSelected)
            ]
            (Html.option [] [ Html.text emptyValue ]
                :: (allMonths
                        |> List.map (\m -> Html.option [] [ Html.text <| Timeline.API.monthToNumeral m ])
                   )
            )
        , case maxDays of
            Nothing ->
                Html.text ""

            Just d ->
                case config.timepoint of
                    YearMonthDay year month currentDay ->
                        Html.select
                            [ Events.onInput (dayListener year month >> config.onSelected)
                            ]
                            (daysOption currentDay d)

                    YearMonth year month ->
                        Html.select
                            [ Attrs.value emptyValue
                            , Events.onInput (dayListener year month >> config.onSelected)
                            ]
                            (daysOption 0 d)

                    _ ->
                        Html.text ""
        ]


viewNewTimeline : Period -> String -> Maybe Int -> Html Msg
viewNewTimeline period name isEditingId =
    let
        ( message, buttonLabel ) =
            case isEditingId of
                Just id ->
                    ( SaveTimeline (Just id) period name, "Save" )

                Nothing ->
                    ( SaveTimeline Nothing period name, "New" )
    in
    Html.div [ Attrs.class "flex gap-4" ]
        [ Html.input [ Events.onInput UpdateNewTimelineName, Attrs.value name ] []
        , Html.select
            [ Events.onInput
                (\t ->
                    UpdatePeriod
                        (case t of
                            "Point" ->
                                Point (Timeline.API.firstTimePointOfPeriod period)

                            "Closed" ->
                                Closed (Timeline.API.firstTimePointOfPeriod period) (Timeline.API.firstTimePointOfPeriod period)

                            "Started" ->
                                Started (Timeline.API.firstTimePointOfPeriod period)

                            "Finished" ->
                                Finished (Timeline.API.firstTimePointOfPeriod period)

                            _ ->
                                period
                        )
                )
            ]
            [ Html.option [ Attrs.selected (Timeline.API.periodToPeriodType period == PointType) ] [ Html.text "Point" ]
            , Html.option
                [ Attrs.selected (Timeline.API.periodToPeriodType period == ClosedType)
                ]
                [ Html.text "Closed" ]
            , Html.option [ Attrs.selected (Timeline.API.periodToPeriodType period == StartedType) ] [ Html.text "Started" ]
            , Html.option [ Attrs.selected (Timeline.API.periodToPeriodType period == FinishedType) ] [ Html.text "Finished" ]
            ]
        , case period of
            Point tp ->
                viewTimepointSelector { timepoint = tp, onSelected = Point >> UpdatePeriod }

            Closed t1 t2 ->
                Html.div [ Attrs.class "flex gap-4" ]
                    [ viewTimepointSelector { timepoint = t1, onSelected = \s -> UpdatePeriod (Closed s t2) }
                    , viewTimepointSelector { timepoint = t2, onSelected = \s -> UpdatePeriod (Closed t1 s) }
                    ]

            Started timePoint ->
                viewTimepointSelector { timepoint = timePoint, onSelected = Started >> UpdatePeriod }

            Finished timePoint ->
                viewTimepointSelector { timepoint = timePoint, onSelected = Finished >> UpdatePeriod }

        --, viewTimepointSelector { timepoint = firstTimePointOfPeriod period, onSelected = UpdateNewTimelinePeriod }
        , Html.button [ Events.onClick message ] [ Html.text buttonLabel ]
        ]
