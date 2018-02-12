module Popup exposing (main)

import Date exposing (Date, Day(..))
import Date.Extra exposing (Interval(..), floor, ceiling, isBetween)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, int, string, field, list)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline as JDP exposing (decode, required)
import String exposing (padLeft)
import Task

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL


type alias Model =
    { date : Maybe Date
    , offset : Int
    , broadcasts : BroadcastState
    }
    
type BroadcastState = Fetching | Failed Http.Error | Success Broadcasts


init : ( Model, Cmd Msg )
init =
    ( Model Nothing 0 Fetching
    , requestInit 
    )

-- BROADCAST

type alias Broadcast =
    { id : Int
    , start : Date
    , end : Date
    , topic : String
    }

type alias Broadcasts = List Broadcast

toViewBroadcast : Date -> Broadcast -> ViewBroadcast
toViewBroadcast time {start, end, topic} =
    let
        now = Date.Extra.isBetween start end time
    in
        ViewBroadcast (formatTimeRange start end) topic now

type alias ViewBroadcast =
    { time : String
    , topic : String
    , now : Bool
    }

type alias ViewBroadcasts = List ViewBroadcast


broadcastsDecoder : Decoder Broadcasts
broadcastsDecoder =
    (field "data" (Json.Decode.list broadcastDecoder))

broadcastDecoder : Decoder Broadcast
broadcastDecoder =
  JDP.decode Broadcast
    |> JDP.required "id" int
    |> JDP.required "start" date
    |> JDP.required "end" date 
    |> JDP.required "topic" string

-- UPDATE


type Msg
    = NextDay
    | PrevDay
    | ReceiveInitialDate Date
    | ReceiveDate Date
    | FetchBroadcasts
    | BroadcastResponse (Result Http.Error Broadcasts)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextDay ->
            case model.date of
                Nothing -> 
                    ( model, Cmd.none )
                Just date -> 
                    ( { model | offset = model.offset + 1 }, Cmd.none )
        
        PrevDay -> 
            case model.date of
                Nothing -> 
                    ( model, Cmd.none )
                Just date -> 
                    ( { model | offset = model.offset - 1 }, Cmd.none )
    
        ReceiveInitialDate date ->
            ( { model | date = Just date }, requestBroadcasts )
    
        ReceiveDate date ->
            ( { model | date = Just date }, Cmd.none )

        FetchBroadcasts ->
            ( { model | broadcasts = Fetching }, requestBroadcasts )

        BroadcastResponse (Ok broadcasts) ->
            ( { model | broadcasts = Success broadcasts }, Cmd.none )

        BroadcastResponse (Err error) ->
            ( { model | broadcasts = Failed error }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        dateString = Maybe.map (\date -> date |> addDays model.offset |> formatDate) model.date |> Maybe.withDefault ""
    in    
        case model.broadcasts of            
            Fetching ->
                viewContainer dateString (viewMessage "Programm wird geladen...")

            Success broadcasts ->
                case model.date of
                    Nothing -> 
                        viewContainer dateString (viewMessage "Programm wird geladen...")
                    Just date ->    
                        let
                            offsetDate = addDays model.offset date
                            todaysBroadcasts = filterBroadcasts offsetDate broadcasts 
                            viewBroadcasts = List.map (toViewBroadcast date) todaysBroadcasts
                        in
                            viewContainer dateString (viewBroadcastTable viewBroadcasts)

            Failed error ->
                viewContainer dateString (viewMessage "Serveranfrage fehlgeschlagen!")

viewContainer : String -> Html Msg -> Html Msg
viewContainer dateString content =
    div [ class "container"] 
        [ div [ class "header" ] 
            [ img [ src "../images/bonjwa.jpg", alt "Bonjwa Logo" ] []
            , span [] [ text "BONJWA PROGRAMM" ]   
            ]
        , div [ class "nav" ] 
            [ div [ id "prev", onClick PrevDay ] [ span [ class "prev" ] [] ]
            , div [ id "day" ] [ text dateString ]
            , div [ id "next", onClick NextDay ] [ span [ class "next" ] [] ]
            ]
        , content
        ]

viewNextButton : Html Msg
viewNextButton =
    button [ onClick NextDay ] [ text "Next" ]                
                
viewPrevButton : Html Msg
viewPrevButton =
    button [ onClick PrevDay ] [ text "Previous" ]
    
viewRefreshButton : Html Msg
viewRefreshButton =
    button [ onClick FetchBroadcasts ] [ text "Refresh" ]

viewMessage : String -> Html Msg
viewMessage message =
    div [ id "status" ] [ text message ]

viewBroadcastTable : ViewBroadcasts -> Html Msg
viewBroadcastTable broadcasts =
    let
        rows = List.map viewBroadcastRow broadcasts
    in
        div [ id "table" ] rows
                
viewBroadcastRow : ViewBroadcast -> Html Msg
viewBroadcastRow { time, topic, now } =
    let
        rowClass = String.join " " <| ["row"] ++ if now then ["live"] else []
    in
        div [ class rowClass ] 
            [ div [ class "left" ] [ div [ class "time" ] [ text time ] ]
            , div [ class "right" ] [ div [ class "topic" ] [ text topic ] ]
            ]
                 
filterBroadcasts : Date -> Broadcasts -> Broadcasts   
filterBroadcasts today broadcasts =
    let
        ceilingDate = Date.Extra.ceiling Day today
        floorDate = Date.Extra.floor Day today
        isToday = Date.Extra.isBetween floorDate ceilingDate
    in
        List.filter (\{start, end} -> isToday start && isToday end) broadcasts 
  

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- DATE

requestInit : Cmd Msg
requestInit =
    Task.perform ReceiveInitialDate Date.now

requestDate : Cmd Msg
requestDate =
    Task.perform ReceiveDate Date.now
    
addDays : Int -> Date -> Date
addDays days date =
    Date.Extra.add Day days date

addDay : Date -> Date
addDay date =
    Date.Extra.add Day 1 date
    
subtractDay : Date -> Date
subtractDay date =
    Date.Extra.add Day (-1) date 
    
formatTimeRange : Date -> Date -> String
formatTimeRange start end =
    let
        startTime = formatTime start
        endTime = formatTime end
    in
        startTime ++ " - " ++ endTime

formatTime : Date -> String
formatTime date =
    let
        hour = padLeft 2 '0' <| toString <| Date.hour date
        minute = padLeft 2 '0' <| toString <| Date.minute date
    in
        hour ++ ":" ++ minute    

formatDate : Date -> String
formatDate date =
    let
        dayName = formatDateDayName date
        day = toString <| Date.day date
        month = toString <| Date.Extra.monthNumber date
        year = toString <| Date.year date
    in
        dayName ++ ", " ++ day ++ "." ++ month ++ "." ++ year    

formatDateDayName : Date -> String
formatDateDayName date =
    case Date.dayOfWeek date of
        Mon -> "Montag"
        Tue -> "Dienstag"
        Wed -> "Mittwoch"
        Thu -> "Donnerstag"
        Fri -> "Freitag"
        Sat -> "Samstag"
        Sun -> "Sonntag"


-- HTTP


requestBroadcasts : Cmd Msg
requestBroadcasts =
    let
        url =
            "https://bnjw.viceair.com/broadcasts"
    in
    Http.send BroadcastResponse (Http.get url broadcastsDecoder)
