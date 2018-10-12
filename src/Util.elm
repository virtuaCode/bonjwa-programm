module Util exposing (addDay, addDays, compareDate, dateEqual, formatDate, formatDateDayName, formatDuration, formatTime, formatTimeRange, isDateBetween, pair, pairLeft, srcset, subtractDay, toMonthNumber)

import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Json.Encode
import String exposing (fromInt, padLeft)
import Time exposing (Month(..), Weekday(..), toDay, toHour, toMinute, toMonth, toWeekday, toYear, utc)
import Time.Extra exposing (Interval(..))


srcset : List String -> Attribute a
srcset items =
    let
        maps =
            items
                |> List.indexedMap (\i item -> String.join "" [ item, " ", fromInt (i + 1), "x" ])
    in
    property "srcset" (maps |> String.join "," >> Json.Encode.string)


pair : a -> b -> ( a, b )
pair a b =
    ( a, b )


pairLeft : a -> b -> c -> ( ( a, b ), c )
pairLeft a b c =
    ( ( a, b ), c )


compareDate : Time.Zone -> Time.Posix -> Time.Posix -> Order
compareDate zone a b =
    let
        difference =
            Time.Extra.diff Millisecond zone b a
    in
    if difference > 0 then
        GT

    else if difference == 0 then
        EQ

    else
        LT


addDays : Int -> Time.Zone -> Time.Posix -> Time.Posix
addDays days zone date =
    Time.Extra.add Day days zone date


addDay : Time.Zone -> Time.Posix -> Time.Posix
addDay zone date =
    addDays 1 zone date


subtractDay : Time.Zone -> Time.Posix -> Time.Posix
subtractDay zone date =
    addDays -1 zone date


isDateBetween : Time.Zone -> Time.Posix -> Time.Posix -> Time.Posix -> Bool
isDateBetween zone lower upper date =
    let
        lowerDiff =
            Time.Extra.diff Millisecond zone lower date

        upperDiff =
            Time.Extra.diff Millisecond zone upper date
    in
    lowerDiff > 0 && upperDiff < 0


dateEqual : Time.Zone -> Time.Posix -> Time.Posix -> Bool
dateEqual zone x y =
    let
        tripleX =
            ( toDay zone x, toMonth zone x, toYear zone x )

        tripleY =
            ( toDay zone y, toMonth zone y, toYear zone y )
    in
    tripleX == tripleY


formatTimeRange : Time.Zone -> Time.Posix -> Time.Posix -> String
formatTimeRange zone start end =
    let
        startTime =
            formatTime zone start

        endTime =
            formatTime zone end
    in
    startTime ++ " - " ++ endTime


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone date =
    let
        hour =
            toHour zone date |> fromInt |> padLeft 2 '0'

        minute =
            toMinute zone date |> fromInt |> padLeft 2 '0'
    in
    hour ++ ":" ++ minute


formatDate : Time.Zone -> Time.Posix -> String
formatDate zone date =
    let
        dayName =
            formatDateDayName zone date

        day =
            toDay zone date |> fromInt

        month =
            toMonth zone date |> toMonthNumber |> fromInt

        year =
            toYear zone date |> fromInt
    in
    dayName ++ ", " ++ day ++ "." ++ month ++ "." ++ year


formatDateDayName : Time.Zone -> Time.Posix -> String
formatDateDayName zone date =
    case toWeekday zone date of
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


toMonthNumber : Month -> Int
toMonthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


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
                ( Just 0, Just 0 ) ->
                    "0 Min."

                ( Just 0, Just minutes ) ->
                    unwords [ fromInt minutes, "Min." ]

                ( Just hours, Just 0 ) ->
                    unwords [ fromInt hours, "Std." ]

                ( Just hours, Just minutes ) ->
                    unwords [ fromInt hours, "Std.", fromInt minutes, "Min." ]

                _ ->
                    duration

        _ ->
            duration
