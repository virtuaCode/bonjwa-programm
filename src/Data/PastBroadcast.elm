module Data.PastBroadcast exposing (..)

import Date exposing (Date)
import Json.Decode exposing (Decoder, field, int, list, string)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (WebData)


-- PastBroadcast


type alias PastBroadcast =
    { date : Date
    , game : String
    , mods : String
    , link : String
    , duration : String
    }


type alias PastBroadcasts =
    List PastBroadcast


type alias PastBroadcastsWebData =
    WebData PastBroadcasts



-- DECODERS


pastBroadcastsDecoder : Decoder (List PastBroadcast)
pastBroadcastsDecoder =
    list pastBroadcastDecoder


pastBroadcastDecoder : Decoder PastBroadcast
pastBroadcastDecoder =
    decode PastBroadcast
        |> required "date" date
        |> required "game" string
        |> required "mods" string
        |> required "link" string
        |> required "duration" string
