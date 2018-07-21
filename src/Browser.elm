port module Browser exposing (..)

import Json.Decode exposing (Value, nullable, float, decodeValue)
import Data.AlarmConfig exposing (AlarmConfig)


port openTab : String -> Cmd msg


port getAlarm : () -> Cmd msg


port setAlarm : AlarmConfig -> Cmd msg


port clearAlarm : () -> Cmd msg


port receiveAlarm : (Value -> msg) -> Sub msg


decodeAlarmValue : (Result String (Maybe Float) -> msg) -> Value -> msg
decodeAlarmValue resultToMsg value =
    let
        result =
            decodeValue (nullable float) value
    in
        resultToMsg result
