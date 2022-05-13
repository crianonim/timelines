module Timeline.Timeline exposing (..)

import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes exposing (href, style, title)


type alias Year =
    Int


type Period
    = Point Year
    | Closed Year Year
    | Started Year


type alias Viewport =
    { start : Year
    , end : Year
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
           )


data =
    [ Timeline (Point 1980) "Jan's Birthday"
    , Timeline (Closed 1995 1999) "High School"
    , Timeline (Started 2019) "Working in Permutive"
    , Timeline (Started 1952) "Queen Elizabeth II reign"
    , Timeline (Started 2005) "Moved to the UK"
    , Timeline (Closed 1789 1795) "French Revolution"
    ]



--
--lowEndViewport : Viewport -> Period -> Year
--lowEndViewport vp p=
--    case p of
--        Point year ->
--            year
--
--        Closed start _ ->
--            start
--
--        Started year ->
--            year
--
--
--
--highEndViewport : Viewport -> Period -> Year
--highEndViewport vp p =
--    case p of
--        Point year ->
--            year
--
--        Closed _ year ->
--            year
--
--        Started _ ->
--            vp.end


isInViewport : Viewport -> Timeline -> Bool
isInViewport { start, end } timeline =
    case timeline.period of
        Point year ->
            year <= end && year >= start

        Closed startYear endYear ->
            startYear >= start && startYear <= end || endYear >= start && endYear <= end

        Started year ->
            year <= end



--
--bothEndsFramed : Viewport -> Period -> ( Year, Year )
--bothEndsFramed frame p =
--    ( lowEndViewport p, highEndViewport frame p )
--
--timeLinesPeriod : List Timeline -> ( Year, Year )
--timeLinesPeriod tls =
--    tls
--        |> List.map .period
--        |> List.foldl
--            (\period ( s1, s2 ) ->
--                let
--                    ( low, high ) =
--                        bothEndsFramed 2021 period
--                in
--                ( min s1 low, max s2 high )
--            )
--            ( 5000, -5000 )
--
--timeLinesViewport : ( Year, Year ) -> ( Year, Year ) -> ( Year, Year )
--timeLinesViewport ( vStart, vEnd ) ( start, end ) =
--    ( min vStart start, max vEnd end )
--
--timeLineExtent : Year -> Year -> Timeline -> ( Float, Float )
--timeLineExtent wholeStart wholeEnd { period } =
--    let
--        duration =
--            end - start
--
--        ( start, end ) =
--            bothEndsFramed 2021 period
--
--        wholeDuration =
--            toFloat
--                (wholeEnd - wholeStart)
--
--        _ =
--            Debug.log "start" [ wholeStart, wholeEnd, start, end ]
--
--        relStart =
--            start - wholeStart
--
--        proportion x =
--            toFloat x / wholeDuration
--    in
--    Tuple.mapBoth proportion proportion ( relStart, duration )


exampleVP : Viewport
exampleVP =
    { start = 1980
    , end = 2021
    }



--example =
--    let
--        ( start, end ) =
--            data
--                |> timeLinesPeriod
--                |> timeLinesViewport ( 1900, 2021 )
--    in
--    data
--        |> List.map (\t -> ( t, timeLineExtent start end t ))


viewTimeline : Timeline -> Html msg
viewTimeline tl =
    div [] [ text <| toString tl ]



--
--viewBar : Timeline -> ( Float, Float ) -> Float -> Html msg
--viewBar { name } ( start, length ) width =
--    div
--        [ style "margin-left" (String.fromFloat (start * width) ++ "px")
--        , style "width" (String.fromFloat (length * width) ++ "px")
--        , style "background-color" "blue"
--        , style "height" "1em"
--        , style "border" "1px solid black"
--        , title name
--        ]
--        []
