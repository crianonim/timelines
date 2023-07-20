module Timeline.API exposing (..)

import Date exposing (Date)
import Http
import Json.Decode
import Json.Encode
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


type PeriodType
    = PointType
    | ClosedType
    | StartedType
    | FinishedType


type alias Viewport =
    { start : TimePoint
    , end : TimePoint
    }


type alias Timeline =
    { id : Int
    , period : Period
    , name : String
    }


type alias Era =
    { name : String
    , viewPort : Viewport
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


minTimePointOfPeriod : Period -> Maybe TimePoint
minTimePointOfPeriod period =
    case period of
        Point timePoint ->
            Just timePoint

        Closed timePoint _ ->
            Just timePoint

        Started timePoint ->
            Just timePoint

        Finished _ ->
            Nothing


maxTimePointOfPeriod : Period -> Maybe TimePoint
maxTimePointOfPeriod period =
    case period of
        Point timePoint ->
            Just timePoint

        Closed _ timePoint ->
            Just timePoint

        Started _ ->
            Nothing

        Finished timePoint ->
            Just timePoint


startOfListOfPeriods : List Period -> Maybe TimePoint
startOfListOfPeriods periods =
    periods
        |> List.map minTimePointOfPeriod
        |> List.foldl
            (\x y ->
                case ( x, y ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Just a, Nothing ) ->
                        Just a

                    ( Nothing, Just a ) ->
                        Just a

                    ( Just a, Just b ) ->
                        Just <| timepointMin a b
            )
            Nothing


endOfListOfPeriods : List Period -> Maybe TimePoint
endOfListOfPeriods periods =
    periods
        |> List.map maxTimePointOfPeriod
        |> List.foldl
            (\x y ->
                case ( x, y ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Just a, Nothing ) ->
                        Just a

                    ( Nothing, Just a ) ->
                        Just a

                    ( Just a, Just b ) ->
                        Just <| timepointMax a b
            )
            Nothing


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



--- ERAS


decodeEra : Json.Decode.Decoder Era
decodeEra =
    Json.Decode.map2 Era
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "viewPort" viewportDecoder)


timePointDecoder : Json.Decode.Decoder TimePoint
timePointDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (stringToTimePoint
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "can't decode TimePoint")
            )


encodeTimePoint : TimePoint -> Json.Encode.Value
encodeTimePoint =
    timePointToString >> Json.Encode.string


encodeViewPort : Viewport -> Json.Encode.Value
encodeViewPort viewport =
    Json.Encode.object <|
        [ ( "start", encodeTimePoint viewport.start )
        , ( "end", encodeTimePoint viewport.end )
        ]


encodeEra : Era -> Json.Encode.Value
encodeEra era =
    Json.Encode.object <|
        [ ( "name", Json.Encode.string era.name )
        , ( "viewPort", encodeViewPort era.viewPort )
        ]


viewportDecoder : Json.Decode.Decoder Viewport
viewportDecoder =
    Json.Decode.map2 Viewport
        (Json.Decode.field "start" timePointDecoder)
        (Json.Decode.field "end" timePointDecoder)


saveNewEra : Era -> (Result Http.Error Era -> msg) -> Cmd msg
saveNewEra era wrapMsg =
    Http.post
        { url = api ++ "/eras"
        , body = Http.jsonBody <| Json.Encode.object [ ( "era", encodeEra era ) ]
        , expect = Http.expectJson wrapMsg decodeEra
        }
