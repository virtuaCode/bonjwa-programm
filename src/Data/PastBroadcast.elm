module Data.PastBroadcast exposing (PastBroadcast, PastBroadcasts, PastBroadcastsWebData, pastBroadcastDecoder, pastBroadcastsDecoder)

import Json.Decode exposing (Decoder, field, int, list, string, succeed)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (WebData)
import Time



-- PastBroadcast


type alias PastBroadcast =
    { date : Time.Posix
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
    succeed PastBroadcast
        |> required "date" datetime
        |> required "game" string
        |> required "mods" string
        |> required "link" string
        |> required "duration" string
