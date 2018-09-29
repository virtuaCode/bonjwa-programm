port module BrowserInterface exposing (clearAlarm, decodeAlarmValue, getAlarm, openTab, receiveAlarm, setAlarm)

import Data.AlarmConfig exposing (AlarmConfig)
import Json.Decode exposing (Error, Value, decodeValue, int, nullable)


port openTab : String -> Cmd msg


port getAlarm : () -> Cmd msg


port setAlarm : AlarmConfig -> Cmd msg


port clearAlarm : () -> Cmd msg


port receiveAlarm : (Value -> msg) -> Sub msg


decodeAlarmValue : (Result Error (Maybe Int) -> msg) -> Value -> msg
decodeAlarmValue resultToMsg value =
    let
        result =
            decodeValue (nullable int) value
    in
    resultToMsg result
