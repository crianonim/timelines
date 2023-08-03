module Timeline.Timeline exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, a, div, h1, h2, text)
import Html.Attributes as Attrs exposing (href, style, title)
import Html.Events as Events
import Http
import Time
import Timeline.API exposing (Era, Period(..), PeriodType(..), TimePoint(..), Timeline, Viewport)


type alias Model =
    { timelines : List Timeline
    , viewPort : Viewport
    , newTimelinePeriod : Period
    , newTimelineName : String
    , isEditingId : Maybe Int
    , eras : List Era
    , selectedEra : Maybe Era
    , newEraName : String
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
    | SelectEra String
    | UpdateNewEraName String
    | AddNewEra
    | SavedNewEra (Result Http.Error Era)
    | GotEras (Result Http.Error (List Era))


init : ( Model, Cmd Msg )
init =
    ( { timelines = []
      , viewPort = exampleVP
      , newTimelinePeriod = Point <| Year 2023
      , newTimelineName = ""
      , isEditingId = Nothing
      , eras = []
      , selectedEra = Nothing
      , newEraName = ""
      }
    , Cmd.batch [ Timeline.API.getTimelines GotTimelines, Timeline.API.getEras GotEras ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStart s ->
            let
                vp =
                    model.viewPort
            in
            ( { model
                | viewPort = { vp | start = Timeline.API.timepointMin s vp.end }
                , selectedEra = Nothing
              }
            , Cmd.none
            )

        UpdateEnd s ->
            let
                vp =
                    model.viewPort
            in
            ( { model
                | viewPort = { vp | end = Timeline.API.timepointMax s vp.start }
                , selectedEra = Nothing
              }
            , Cmd.none
            )

        SaveTimeline mId period name ->
            ( model
            , case mId of
                Nothing ->
                    Timeline.API.saveNewTimeline { id = 0, name = name, period = period } (SavedTimeline mId)

                Just id ->
                    Timeline.API.saveEditTimeline { id = id, name = name, period = period } (SavedTimeline mId)
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
            ( model, Timeline.API.removeTimeline id (RemovedTimeline id) )

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

        SelectEra eraId ->
            let
                era =
                    model.eras
                        |> List.filter
                            (\e -> Just e.id == String.toInt eraId)
                        |> List.head
            in
            ( { model | selectedEra = era, viewPort = Maybe.map .viewPort era |> Maybe.withDefault model.viewPort }
            , Cmd.none
            )

        AddNewEra ->
            let
                era =
                    { name = model.newEraName
                    , viewPort = model.viewPort
                    , id = -100
                    }
            in
            ( { model | eras = model.eras ++ [ era ] }, Timeline.API.saveNewEra era SavedNewEra )

        UpdateNewEraName string ->
            ( { model | newEraName = string }, Cmd.none )

        SavedNewEra result ->
            let
                _ =
                    Debug.log "SAVED new era" result
            in
            ( model, Cmd.none )

        GotEras (Ok eras) ->
            ( { model | eras = eras }, Cmd.none )

        GotEras (Err err) ->
            let
                _ =
                    Debug.log "Error in getting eras" err
            in
            ( model, Cmd.none )


type alias TimeLineBar =
    { start : Maybe Float
    , length : Float
    , timeline : Timeline
    }


data : List Timeline
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
            , viewEras model.eras model.selectedEra model.newEraName
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


allMonths : List Date.Month
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


viewEras : List Era -> Maybe Era -> String -> Html Msg
viewEras eras maybeEra newEraName =
    Html.div []
        [ Html.select [ Events.onInput SelectEra ]
            (Html.option [] [ Html.text "--" ]
                :: List.map
                    (\e ->
                        Html.option
                            [ Attrs.value <| String.fromInt e.id
                            , Attrs.selected (Just e == maybeEra)
                            ]
                            [ Html.text e.name ]
                    )
                    eras
            )
        , Html.input [ Events.onInput UpdateNewEraName, Attrs.value newEraName ] []
        , Html.button [ Events.onClick AddNewEra ] [ Html.text "New Era" ]
        ]
