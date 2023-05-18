module Timeline.Timeline exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes as Attrs exposing (href, style, title)
import Time


type TimePoint
    = Year Int
    | YearMonth Int Time.Month
    | YearMonthDay Int Time.Month Int


type Period
    = Point TimePoint
    | Closed TimePoint TimePoint
    | Started TimePoint
    | Finished TimePoint


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
    { period : Period
    , name : String
    }


monthToNumeral : Time.Month -> String
monthToNumeral month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


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
            [ String.fromInt year, monthToNumeral month, String.fromInt day ] |> String.join "-"


toString : Timeline -> String
toString { period, name } =
    name
        ++ (case period of
                Point point ->
                    timePointToString point

                Closed from to ->
                    timePointToString from ++ " - " ++ timePointToString to

                Started startPoint ->
                    timePointToString startPoint ++ " - "

                Finished endPoint ->
                    " - " ++ timePointToString endPoint
           )


data =
    [ Timeline (Point <| YearMonthDay 1980 Time.Jun 6) "Jan's Birthday"
    , Timeline (Closed (Year 1995) (Year 1999)) "High School"
    , Timeline (Started (Year 2019)) "Working in Permutive"
    , Timeline (Closed (Year 1952) (Year 2022)) "Queen Elizabeth II reign"
    , Timeline (Started (Year 2005)) "Moved to the UK"
    , Timeline (Closed (Year 1789) (Year 1795)) "French Revolution"
    , Timeline (Closed (Year 2014) (Year 2018)) "Lived in Clapham"
    , Timeline (Finished (Year 1985)) "Finished in 1985"
    , Timeline (Point <| YearMonthDay 2023 Time.Apr 6) "Left Permutive"
    , Timeline (Point <| Year 2022) "Bad Year"
    , Timeline (Point <| YearMonth 2023 Time.Jun) "Pride Month"
    ]


isInViewport : Viewport -> Period -> Bool
isInViewport { start, end } period =
    let
        viewportStart =
            timePointToStartDate start
                |> Debug.log "VPStart"

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
            ((Date.compare periodEnd viewportStart == GT || Date.compare periodEnd viewportStart == EQ)
                && (Date.compare periodEnd viewportEnd == LT || Date.compare periodEnd viewportEnd == EQ)
            )
                || ((Date.compare periodStart viewportStart == GT || Date.compare periodStart viewportStart == EQ)
                        && (Date.compare periodStart viewportEnd == LT || Date.compare periodStart viewportEnd == EQ)
                   )

        --startYear >= start && startYear <= end || endYear >= start && endYear <= end
        Started tp ->
            let
                periodStart =
                    timePointToStartDate tp
            in
            Date.compare periodStart viewportEnd == GT || Date.compare periodStart viewportStart == EQ

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
                    ( timePointToStartDate y1, timePointToEndDate y2 )

                Started tp ->
                    ( timePointToStartDate tp, viewportEnd )

                Finished tp ->
                    ( viewportStart, timePointToEndDate tp )

        viewPortDays =
            Date.diff Date.Days viewportStart viewportEnd
                |> Debug.log "Days"

        scale =
            width
                / toFloat viewPortDays
                |> Debug.log "Scale"
    in
    { start =
        if Date.compare viewportStart dateStart == GT || isPeriodFinished period then
            Nothing

        else
            Just (toFloat (Date.diff Date.Days viewportStart dateStart) * scale)
    , length = toFloat (Date.diff Date.Days (Date.max viewportStart dateStart) dateEnd) * scale
    , timeline = tl
    }
        |> Debug.log "TimeLineBar"


exampleVP : Viewport
exampleVP =
    { start = Year 2020
    , end = Year 2023
    }


viewTimeline : Timeline -> Html msg
viewTimeline tl =
    div [] [ text <| toString tl ]


viewBar : Float -> TimeLineBar -> Html msg
viewBar width { timeline, start, length } =
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

        _ =
            Debug.log "start" start
    in
    div
        [ style "margin-left" (String.fromFloat (startPoint * width) ++ "px")
        , style "width" (String.fromFloat (length * width) ++ "px")
        , Attrs.class "bg-sky-500 h-4 border border-black"
        , endStyle
        , startStyle
        , title timeline.name
        ]
        []
