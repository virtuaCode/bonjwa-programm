module Data.Broadcast exposing
    ( Broadcast
    , Broadcasts
    , BroadcastsWebData
    , broadcastDecoder
    , broadcastsDecoder
    )

import Json.Decode exposing (Decoder, field, int, list, string, succeed)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)
import Time



-- Broadcast


type alias Broadcast =
    { id : Int
    , start : Time.Posix
    , end : Time.Posix
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
    succeed Broadcast
        |> required "id" int
        |> required "start" datetime
        |> required "end" datetime
        |> required "topic" string
        |> required "game" string
        |> required "streamers" string
