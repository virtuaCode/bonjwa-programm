module Util exposing (..)

import Date exposing (Date, Day(..))
import Date.Extra exposing (Interval(..), add)
import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Json.Encode
import String exposing (padLeft)


srcset : List String -> Attribute a
srcset items =
    let
        maps =
            items
                |> List.indexedMap (\i item -> String.join "" [ item, " ", toString (i + 1), "x" ])
    in
    property "srcset" (maps |> String.join "," >> Json.Encode.string)


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


addDays : Int -> Date -> Date
addDays days date =
    Date.Extra.add Day days date


addDay : Date -> Date
addDay date =
    Date.Extra.add Day 1 date


subtractDay : Date -> Date
subtractDay date =
    Date.Extra.add Day -1 date


dateEqual : Date -> Date -> Bool
dateEqual x y =
    let
        tripleX =
            ( Date.day x, Date.month x, Date.year x )

        tripleY =
            ( Date.day y, Date.month y, Date.year y )
    in
    tripleX == tripleY


formatTimeRange : Date -> Date -> String
formatTimeRange start end =
    let
        startTime =
            formatTime start

        endTime =
            formatTime end
    in
    startTime ++ " - " ++ endTime


formatTime : Date -> String
formatTime date =
    let
        hour =
            padLeft 2 '0' <| toString <| Date.hour date

        minute =
            padLeft 2 '0' <| toString <| Date.minute date
    in
    hour ++ ":" ++ minute


formatDate : Date -> String
formatDate date =
    let
        dayName =
            formatDateDayName date

        day =
            toString <| Date.day date

        month =
            toString <| Date.Extra.monthNumber date

        year =
            toString <| Date.year date
    in
    dayName ++ ", " ++ day ++ "." ++ month ++ "." ++ year


formatDateDayName : Date -> String
formatDateDayName date =
    case Date.dayOfWeek date of
        Mon ->
            "Montag"

        Tue ->
            "Dienstag"

        Wed ->
            "Mittwoch"

        Thu ->
            "Donnerstag"

        Fri ->
            "Freitag"

        Sat ->
            "Samstag"

        Sun ->
            "Sonntag"


formatDuration : String -> String
formatDuration duration =
    case String.split ":" duration of
        [ hh, mm ] ->
            let
                intHours =
                    String.toInt hh

                intMinutes =
                    String.toInt mm

                unwords =
                    String.join " "
            in
            case ( intHours, intMinutes ) of
                ( Ok 0, Ok 0 ) ->
                    "0 Min."

                ( Ok 0, Ok minutes ) ->
                    unwords [ toString minutes, "Min." ]

                ( Ok hours, Ok 0 ) ->
                    unwords [ toString hours, "Std." ]

                ( Ok hours, Ok minutes ) ->
                    unwords [ toString hours, "Std.", toString minutes, "Min." ]

                _ ->
                    duration

        _ ->
            duration
