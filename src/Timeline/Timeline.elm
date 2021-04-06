module Timeline.Timeline exposing (..)

import Html exposing (Attribute, Html, a, div, h1, text)
import Html.Attributes exposing (href)


type alias Year =
    Int


type Period
    = Point Year
    | Closed Year Year


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
           )


data =
    [ Timeline (Point 1980) "Jan's Birthday"
    , Timeline (Closed 1995 1999) "High School"
    ]


viewTimeline : Timeline -> Html msg
viewTimeline tl =
    div [] [ text <| toString tl ]
