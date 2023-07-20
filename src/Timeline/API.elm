module Timeline.API exposing (..)

import Date exposing (Date)
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
