module Timeline.Timeline exposing (..)

import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes as Attrs exposing (href, style, title)


type alias Year =
    Int


type Period
    = Point Year
    | Closed Year Year
    | Started Year
    | Finished Year


type alias Viewport =
    { start : Year
    , end : Year
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


toString : Timeline -> String
toString { period, name } =
    name
        ++ (case period of
                Point year ->
                    String.fromInt year

                Closed from to ->
                    String.fromInt from ++ " - " ++ String.fromInt to

                Started startYear ->
                    String.fromInt startYear ++ " - "

                Finished endYear ->
                    " - " ++ String.fromInt endYear
           )


data =
    [ Timeline (Point 1980) "Jan's Birthday"
    , Timeline (Closed 1995 1999) "High School"
    , Timeline (Started 2019) "Working in Permutive"
    , Timeline (Started 1952) "Queen Elizabeth II reign"
    , Timeline (Started 2005) "Moved to the UK"
    , Timeline (Closed 1789 1795) "French Revolution"
    , Timeline (Closed 2014 2018) "Lived in Clapham"
    , Timeline (Finished 1985) "Finished in 1985"
    ]


isInViewport : Viewport -> Timeline -> Bool
isInViewport { start, end } timeline =
    case timeline.period of
        Point year ->
            year <= end && year >= start

        Closed startYear endYear ->
            startYear >= start && startYear <= end || endYear >= start && endYear <= end

        Started year ->
            year <= end

        Finished year ->
            year >= start


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
        ( yearStart, yearEnd ) =
            case period of
                Point year ->
                    ( year, year )

                Closed y1 y2 ->
                    ( y1, y2 )

                Started year ->
                    ( year, end )

                Finished year ->
                    ( start, year )

        viewPortYears =
            end - start

        scale =
            width / toFloat viewPortYears
    in
    { start =
        if start > yearStart || isPeriodFinished period then
            Nothing

        else
            Just (toFloat (yearStart - start) * scale)
    , length = toFloat (yearEnd - Basics.max yearStart start) * scale
    , timeline = tl
    }


exampleVP : Viewport
exampleVP =
    { start = 1970
    , end = 2022
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
