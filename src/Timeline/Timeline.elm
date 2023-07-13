module Timeline.Timeline exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, a, div, h1, h2, text)
import Html.Attributes as Attrs exposing (href, style, title)
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode
import Time


type alias Model =
    { timelines : List Timeline
    , viewPort : Viewport
    , newTimelinePeriod : Period
    , newTimelineName : String
    }


type Msg
    = UpdateStart TimePoint
    | UpdateEnd TimePoint
    | GotTimelines (Result Http.Error (List Timeline))
    | UpdatePeriod Period
    | UpdateNewTimelinePeriod TimePoint
    | UpdateNewTimelineName String
    | SaveTimeline Timeline
    | SavedTimeline (Result Http.Error Timeline)


init : ( Model, Cmd Msg )
init =
    ( { timelines = [], viewPort = exampleVP, newTimelinePeriod = Point <| Year 2023, newTimelineName = "" }, getTimelines GotTimelines )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStart s ->
            let
                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | start = timepointMin s vp.end } }, Cmd.none )

        UpdateEnd s ->
            let
                vp =
                    model.viewPort
            in
            ( { model | viewPort = { vp | end = timepointMax s vp.start } }, Cmd.none )

        SaveTimeline timeline ->
            ( model, saveTimeline timeline SavedTimeline )

        SavedTimeline (Ok tl) ->
            ( { model | timelines = model.timelines ++ [ tl ] }, Cmd.none )

        SavedTimeline (Err err) ->
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

        UpdateNewTimelinePeriod timePoint ->
            ( { model | newTimelinePeriod = Point timePoint }, Cmd.none )

        UpdateNewTimelineName name ->
            ( { model | newTimelineName = name }, Cmd.none )

        UpdatePeriod period ->
            let
                _ =
                    Debug.log "new period" period
            in
            ( { model | newTimelinePeriod = period }, Cmd.none )


type TimePoint
    = Year Int
    | YearMonth Int Time.Month
    | YearMonthDay Int Time.Month Int


type Period
    = Point TimePoint
    | Closed TimePoint TimePoint
    | Started TimePoint
    | Finished TimePoint


type PeriodType
    = PointType
    | ClosedType
    | StartedType
    | FinishedType


type alias Viewport =
    { start : TimePoint
    , end : TimePoint
    }


type alias TimeLineBar =
    { start : Maybe Float
    , length : Float
    , timeline : Timeline
    }


type alias Timeline =
    { id : Int
    , period : Period
    , name : String
    }


periodToPeriodType : Period -> PeriodType
periodToPeriodType period =
    case period of
        Point _ ->
            PointType

        Closed _ _ ->
            ClosedType

        Started _ ->
            StartedType

        Finished _ ->
            FinishedType


firstTimePointOfPeriod : Period -> TimePoint
firstTimePointOfPeriod period =
    case period of
        Point timePoint ->
            timePoint

        Closed timePoint _ ->
            timePoint

        Started timePoint ->
            timePoint

        Finished timePoint ->
            timePoint


timepointMin : TimePoint -> TimePoint -> TimePoint
timepointMin t1 t2 =
    case Date.compare (timePointToStartDate t1) (timePointToStartDate t2) of
        LT ->
            t1

        EQ ->
            t1

        GT ->
            t2


timepointMax : TimePoint -> TimePoint -> TimePoint
timepointMax t1 t2 =
    case Date.compare (timePointToEndDate t1) (timePointToEndDate t2) of
        LT ->
            t2

        EQ ->
            t2

        GT ->
            t1


timepointToYear : TimePoint -> Int
timepointToYear tp =
    case tp of
        Year int ->
            int

        YearMonth int _ ->
            int

        YearMonthDay int _ _ ->
            int


monthToNumeral : Time.Month -> String
monthToNumeral =
    Date.monthToNumber >> String.fromInt >> String.padLeft 2 '0'


timePointToStartDate : TimePoint -> Date
timePointToStartDate tp =
    case tp of
        Year year ->
            Date.fromCalendarDate year Time.Jan 1

        YearMonth year month ->
            Date.fromCalendarDate year month 1

        YearMonthDay year month day ->
            Date.fromCalendarDate year month day


timePointToEndDate : TimePoint -> Date
timePointToEndDate tp =
    case tp of
        Year year ->
            Date.fromCalendarDate year Time.Dec 31

        YearMonth year month ->
            Date.fromCalendarDate year month 2
                |> Date.ceiling Date.Month
                |> Date.add Date.Days -1

        YearMonthDay year month day ->
            Date.fromCalendarDate year month day


timePointToString : TimePoint -> String
timePointToString timePoint =
    case timePoint of
        Year year ->
            String.fromInt year

        YearMonth year month ->
            [ String.fromInt year, monthToNumeral month ] |> String.join "-"

        YearMonthDay year month day ->
            [ String.fromInt year, monthToNumeral month, String.fromInt day |> String.padLeft 2 '0' ] |> String.join "-"


periodToString : Period -> String
periodToString period =
    case period of
        Point point ->
            timePointToString point

        Closed from to ->
            timePointToString from ++ " - " ++ timePointToString to

        Started startPoint ->
            timePointToString startPoint ++ " - "

        Finished endPoint ->
            " - " ++ timePointToString endPoint


toString : Timeline -> String
toString { period, name } =
    name
        ++ " "
        ++ periodToString period


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
            timePointToStartDate start

        viewportEnd =
            timePointToEndDate end
    in
    case period of
        Point tp ->
            let
                periodStart =
                    timePointToStartDate tp

                periodEnd =
                    timePointToEndDate tp
            in
            (Date.compare periodStart viewportStart == GT || Date.compare periodStart viewportStart == EQ)
                && (Date.compare periodEnd viewportEnd == LT || Date.compare periodEnd viewportEnd == EQ)

        Closed startTP endTP ->
            let
                periodStart =
                    timePointToStartDate startTP

                periodEnd =
                    timePointToEndDate endTP
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
                    timePointToStartDate tp
            in
            Date.compare periodStart viewportEnd == LT || Date.compare periodStart viewportStart == EQ

        Finished tp ->
            let
                periodStart =
                    timePointToStartDate tp
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
            timePointToStartDate start

        viewportEnd =
            timePointToEndDate end

        ( dateStart, dateEnd ) =
            case period of
                Point tp ->
                    ( timePointToStartDate tp, timePointToEndDate tp )

                Closed y1 y2 ->
                    ( timePointToStartDate y1, Date.min (timePointToEndDate y2) viewportEnd )

                Started tp ->
                    ( timePointToStartDate tp, viewportEnd )

                Finished tp ->
                    ( viewportStart, timePointToEndDate tp )

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


viewTimeline : Timeline -> Html msg
viewTimeline tl =
    div [] [ text <| toString tl ]


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


saveTimeline : Timeline -> (Result Http.Error Timeline -> msg) -> Cmd msg
saveTimeline timeline wrapMsg =
    Http.post
        { url = api ++ "/timelines"
        , body = Http.jsonBody <| Json.Encode.object [ ( "timeline", encodeTimeline timeline ) ]
        , expect = Http.expectJson wrapMsg decodeTimeline
        }


encodePeriod : Period -> Json.Encode.Value
encodePeriod period =
    periodToString period |> Json.Encode.string


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
        , div [] [ viewNewTimeline model.newTimelinePeriod model.newTimelineName ]
        , div [ Attrs.class "flex gap-4" ]
            [ div [] [ text "From:" ]
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

                YearMonth int month ->
                    Just <| daysInAMonth int month

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
                    Just <| monthToNumeral month

                YearMonthDay _ month _ ->
                    Just <| monthToNumeral month

        monthListener : String -> TimePoint
        monthListener month =
            String.toInt month
                |> Maybe.map Date.numberToMonth
                |> Maybe.map (\m -> YearMonth (timepointToYear config.timepoint) m)
                |> Maybe.withDefault (Year (timepointToYear config.timepoint))

        dayListener : Int -> Time.Month -> String -> TimePoint
        dayListener year month dayString =
            String.toInt dayString
                |> Maybe.map (\d -> YearMonthDay year month d)
                |> Maybe.withDefault (YearMonth year month)
    in
    Html.div []
        [ Html.input
            [ Attrs.type_ "number"
            , Attrs.class "w-16"
            , Events.onInput (parseYear >> Year >> config.onSelected)
            , Attrs.value <| String.fromInt <| timepointToYear config.timepoint
            ]
            []
        , Html.select
            [ Attrs.value <| Maybe.withDefault emptyValue <| getMonth config.timepoint
            , Events.onInput (monthListener >> config.onSelected)
            ]
            (Html.option [] [ Html.text emptyValue ]
                :: (allMonths
                        |> List.map (\m -> Html.option [] [ Html.text <| monthToNumeral m ])
                   )
            )
        , case maxDays of
            Nothing ->
                Html.text ""

            Just d ->
                (case config.timepoint of
                    YearMonthDay year month currentDay ->
                        Html.select
                            [ Attrs.value <| String.fromInt currentDay
                            , Events.onInput (dayListener year month >> config.onSelected)
                            ]

                    YearMonth year month ->
                        Html.select
                            [ Attrs.value emptyValue
                            , Events.onInput (dayListener year month >> config.onSelected)
                            ]

                    _ ->
                        Html.div []
                )
                    (Html.option [] [ Html.text emptyValue ]
                        :: (List.range 1 d
                                |> List.map (\m -> Html.option [] [ Html.text <| String.fromInt m ])
                           )
                    )
        ]


viewNewTimeline : Period -> String -> Html Msg
viewNewTimeline period name =
    Html.div [ Attrs.class "flex gap-4" ]
        [ Html.input [ Events.onInput UpdateNewTimelineName, Attrs.value name ] []
        , Html.select
            [ Events.onInput
                (\t ->
                    UpdatePeriod
                        (case t of
                            "Point" ->
                                Point (firstTimePointOfPeriod period)

                            "Closed" ->
                                Closed (firstTimePointOfPeriod period) (firstTimePointOfPeriod period)

                            "Started" ->
                                Started (firstTimePointOfPeriod period)

                            "Finished" ->
                                Finished (firstTimePointOfPeriod period)

                            _ ->
                                period
                        )
                )
            ]
            [ Html.option [ Attrs.selected (periodToPeriodType period == PointType) ] [ Html.text "Point" ]
            , Html.option
                [ Attrs.selected (periodToPeriodType period == ClosedType)
                ]
                [ Html.text "Closed" ]
            , Html.option [ Attrs.selected (periodToPeriodType period == StartedType) ] [ Html.text "Started" ]
            , Html.option [ Attrs.selected (periodToPeriodType period == FinishedType) ] [ Html.text "Finished" ]
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
        , Html.button [ Events.onClick <| SaveTimeline { id = 0, name = name, period = period } ] [ Html.text "New" ]
        ]
