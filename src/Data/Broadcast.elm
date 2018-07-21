module Data.Broadcast
    exposing
        ( Broadcast
        , Broadcasts
        , BroadcastsWebData
        , broadcastDecoder
        , broadcastsDecoder
        )

import Date exposing (Date)
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (WebData)


-- Broadcast


type alias Broadcast =
    { id : Int
    , start : Date
    , end : Date
    , topic : String
    , game : String
    , streamers : String
    }


type alias Broadcasts =
    List Broadcast


type alias BroadcastsWebData =
    WebData Broadcasts



-- DECODERS


broadcastsDecoder : Decoder Broadcasts
broadcastsDecoder =
    field "data" (list broadcastDecoder)


broadcastDecoder : Decoder Broadcast
broadcastDecoder =
    decode Broadcast
        |> required "id" int
        |> required "start" date
        |> required "end" date
        |> required "topic" string
        |> required "game" string
        |> required "streamers" string
