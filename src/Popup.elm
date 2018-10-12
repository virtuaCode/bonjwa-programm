module Popup exposing (main)

import Browser
import BrowserInterface exposing (clearAlarm, decodeAlarmValue, getAlarm, openTab, receiveAlarm, setAlarm)
import Data.AlarmConfig exposing (..)
import Data.Broadcast exposing (..)
import Data.Dialog as Dialog exposing (Button, Dialog)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Page.PastBroadcast
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route(..))
import Task
import Time
import Time.Extra exposing (..)
import Util exposing (..)
import Views.Container as Container
import Views.Dialog
import Views.Message as Message


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Page


type Page
    = PastBroadcastsPage Page.PastBroadcast.Model



-- MODEL


type alias Model =
    { date : Maybe Time.Posix
    , zone : Maybe Time.Zone
    , offset : Int
    , broadcasts : BroadcastsWebData
    , subpage : Maybe Page
    , dialog : Maybe (Dialog.Dialog Msg)
    , alarm : Maybe Int
    }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.batch [ getAlarm (), requestInit ] )


initModel : Model
initModel =
    { date = Nothing
    , zone = Nothing
    , offset = 0
    , broadcasts = NotAsked
    , subpage = Nothing
    , dialog = Nothing
    , alarm = Nothing
    }



-- BROADCAST


styleBroadcasts : Time.Zone -> Time.Posix -> Time.Posix -> Maybe Int -> Broadcasts -> List (Styled Broadcast)
styleBroadcasts zone time offsetTime alarm =
    List.map (styleBroadcast zone time offsetTime alarm)


styleBroadcast : Time.Zone -> Time.Posix -> Time.Posix -> Maybe Int -> Broadcast -> Styled Broadcast
styleBroadcast zone time offsetTime alarm broadcast =
    let
        now =
            isTimeBetweenBroadcast zone time broadcast

        spanType =
            getSpanType zone offsetTime broadcast
    in
    if now then
        Styled Live spanType broadcast

    else
        case alarm of
            Nothing ->
                if Time.posixToMillis time > Time.posixToMillis broadcast.start then
                    Styled NoAlarm spanType broadcast

                else
                    Styled Default spanType broadcast

            Just alarmTime ->
                if alarmTime == Time.posixToMillis broadcast.start then
                    Styled Alarm spanType broadcast

                else if Time.posixToMillis time > Time.posixToMillis broadcast.start then
                    Styled NoAlarm spanType broadcast

                else
                    Styled Default spanType broadcast


isTimeBetweenBroadcast : Time.Zone -> Time.Posix -> Broadcast -> Bool
isTimeBetweenBroadcast zone time { start, end } =
    isDateBetween zone start end time


getSpanType : Time.Zone -> Time.Posix -> Broadcast -> Span
getSpanType zone date { start, end } =
    let
        ceilingDate =
            Time.Extra.ceiling Day zone date

        floorDate =
            Time.Extra.floor Day zone date

        startEqual =
            dateEqual zone date start

        endEqual =
            dateEqual zone date end
    in
    case ( not startEqual && floorDate /= start, not endEqual && ceilingDate /= end ) of
        ( True, False ) ->
            Previous

        ( False, True ) ->
            Next

        ( _, _ ) ->
            None


type Styled a
    = Styled Style Span a


type Style
    = Alarm
    | Live
    | Default
    | NoAlarm


type Span
    = None
    | Next
    | Previous


type DialogAction
    = ActionCancel
    | ActionClearAlarm
    | ActionSetAlarm AlarmConfig



-- UPDATE


type Msg
    = NextDay
    | PrevDay
    | ShowDialog (Dialog Msg)
    | DialogMsg DialogAction
    | ReceiveInitialDate ( Time.Zone, Time.Posix )
    | ReceiveDate ( Time.Zone, Time.Posix )
    | ReceiveAlarm (Maybe Int)
    | ReceiveAlarmError Json.Decode.Error
    | ShowRoute Route
    | OpenTab String
    | GetAlarm
    | SetAlarm AlarmConfig
    | PastBroadcastMsg Page.PastBroadcast.Msg
    | BroadcastResponse BroadcastsWebData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.subpage ) of
        ( NextDay, _ ) ->
            case model.date of
                Nothing ->
                    ( model, Cmd.none )

                Just date ->
                    let
                        newModel =
                            { model | offset = model.offset + 1 }
                    in
                    ( newModel, Cmd.none )

        ( PrevDay, _ ) ->
            case model.date of
                Nothing ->
                    ( model, Cmd.none )

                Just date ->
                    let
                        newModel =
                            { model | offset = model.offset - 1 }
                    in
                    ( newModel, Cmd.none )

        ( OpenTab url, _ ) ->
            ( model, openTab url )

        ( GetAlarm, _ ) ->
            ( model, getAlarm () )

        ( SetAlarm config, _ ) ->
            ( model, setAlarm config )

        ( ReceiveAlarm alarm, _ ) ->
            let
                newModel =
                    { model | alarm = alarm }
            in
            ( newModel, Cmd.none )

        ( ReceiveInitialDate ( zone, date ), _ ) ->
            let
                newModel =
                    { model | date = Just date, zone = Just zone }
            in
            ( newModel, requestBroadcasts )

        ( ReceiveDate ( zone, date ), _ ) ->
            let
                newModel =
                    { model | date = Just date, zone = Just zone }
            in
            ( newModel, Cmd.none )

        ( BroadcastResponse data, _ ) ->
            let
                newModel =
                    { model | broadcasts = data }
            in
            ( newModel, Cmd.none )

        ( ShowRoute route, _ ) ->
            showRoute route model

        ( PastBroadcastMsg subMsg, Just (PastBroadcastsPage subModel) ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Page.PastBroadcast.update subMsg subModel

                ( newModel, command ) =
                    case msgFromPage of
                        Page.PastBroadcast.NoOp ->
                            let
                                newPageModel =
                                    { model | subpage = Just (PastBroadcastsPage pageModel) }

                                pageCommand =
                                    Cmd.map PastBroadcastMsg cmd
                            in
                            ( newPageModel, pageCommand )

                        Page.PastBroadcast.OpenTab link ->
                            let
                                newPageModel =
                                    { model | subpage = Just (PastBroadcastsPage pageModel) }

                                pageCommand =
                                    Cmd.batch
                                        [ openTab link
                                        , Cmd.map PastBroadcastMsg cmd
                                        ]
                            in
                            ( newPageModel, pageCommand )

                        Page.PastBroadcast.Back ->
                            let
                                newPageModel =
                                    { model | subpage = Nothing }

                                pageCommand =
                                    Cmd.map PastBroadcastMsg cmd
                            in
                            ( newPageModel, pageCommand )
            in
            ( newModel, command )

        ( ShowDialog dialog, _ ) ->
            let
                newModel =
                    { model | dialog = Just dialog }
            in
            ( newModel, Cmd.none )

        ( DialogMsg dialogMsg, _ ) ->
            let
                newModel =
                    { model | dialog = Nothing }
            in
            case dialogMsg of
                ActionCancel ->
                    ( newModel, Cmd.none )

                ActionSetAlarm config ->
                    ( newModel, setAlarm config )

                ActionClearAlarm ->
                    ( newModel, clearAlarm () )

        ( _, _ ) ->
            ( model, Cmd.none )


showRoute : Route -> Model -> ( Model, Cmd Msg )
showRoute route model =
    case route of
        Route.PastBroadcasts ->
            let
                ( pageModel, cmd ) =
                    Page.PastBroadcast.init

                newModel =
                    { model | subpage = Just (PastBroadcastsPage pageModel) }
            in
            ( newModel, Cmd.map PastBroadcastMsg cmd )



-- VIEW


view : Model -> Html Msg
view model =
    case model.dialog of
        Nothing ->
            case model.subpage of
                Nothing ->
                    viewProgramm model

                Just (PastBroadcastsPage subModel) ->
                    Page.PastBroadcast.view subModel
                        |> Html.map PastBroadcastMsg

        Just dialog ->
            Views.Dialog.viewDialog dialog



-- Programm


viewProgramm : Model -> Html Msg
viewProgramm model =
    let
        dateString =
            case ( model.zone, model.date ) of
                ( Just zone, Just date ) ->
                    addDays model.offset zone date
                        |> formatDate zone

                _ ->
                    ""

        content =
            viewProgrammContent model.zone model.date model.offset model.alarm model.broadcasts
    in
    viewProgrammContainer dateString content


viewProgrammContent : Maybe Time.Zone -> Maybe Time.Posix -> Int -> Maybe Int -> BroadcastsWebData -> Html Msg
viewProgrammContent zone date offset alarm remoteData =
    case ( remoteData, ( zone, date ) ) of
        ( NotAsked, _ ) ->
            Message.view "Programm wird geladen..."

        ( Loading, _ ) ->
            Message.view "Serveranfrage fehlgeschlagen!"

        ( Failure _, _ ) ->
            Message.view "Serveranfrage fehlgeschlagen!"

        ( Success broadcasts, ( Just timezone, Just currentDate ) ) ->
            let
                offsetDate =
                    addDays offset timezone currentDate

                visibleBroadcasts =
                    broadcasts
                        |> filterBroadcasts timezone offsetDate
                        |> sortBroadcasts timezone
                        |> styleBroadcasts timezone currentDate offsetDate alarm
            in
            viewBroadcastTable timezone visibleBroadcasts

        ( Success _, _ ) ->
            Message.view "Programm wird geladen..."


viewProgrammHeader : List (Html Msg)
viewProgrammHeader =
    [ span [ class "title button", title "www.bonjwa.de/programm", onClick (OpenTab "https://www.bonjwa.de/programm") ] [ text "PROGRAMM" ]

    --, span [ class "button button-emote", title "www.bonjwa.de", onClick (OpenTab "https://www.bonjwa.de") ]
    --    [ img [ src "../images/chill_28.png" ] []
    --    ]
    --, span [ class "button", title "Past Broadcasts", onClick (ShowRoute Route.PastBroadcasts) ]
    --    [ img [ src "../images/video_48_1x.png", srcset [ "../images/video_48_1x.png", "../images/video_48_2x.png" ] ] []
    --    ]
    ]


viewProgrammNavigation : String -> List (Html Msg)
viewProgrammNavigation date =
    [ div [ id "prev", title "Vorheriger Tag", onClick PrevDay ] [ span [ class "prev" ] [] ]
    , div [ id "day" ] [ text date ]
    , div [ id "next", title "Nächster Tag", onClick NextDay ] [ span [ class "next" ] [] ]
    ]


viewProgrammContainer : String -> Html Msg -> Html Msg
viewProgrammContainer date content =
    Container.view
        "programm-container"
        viewProgrammHeader
        (viewProgrammNavigation date)
        content


viewBroadcastTable : Time.Zone -> List (Styled Broadcast) -> Html Msg
viewBroadcastTable zone broadcasts =
    let
        rows =
            List.map (viewBroadcastRow zone) broadcasts
    in
    div [ id "table" ] rows


viewBroadcastRow : Time.Zone -> Styled Broadcast -> Html Msg
viewBroadcastRow zone styledBroadcast =
    let
        (Styled style spanType broadcast) =
            styledBroadcast

        { start, end, topic, game, streamers } =
            broadcast

        streamersText =
            if streamers == "" then
                game

            else
                streamers

        timeText =
            formatTimeRange zone start end

        time =
            viewBroadcastTime zone spanType broadcast

        timeClass =
            case spanType of
                Next ->
                    "time time-next-span"

                Previous ->
                    "time time-previous-span"

                None ->
                    "time"

        ( rowElement, indicator ) =
            case style of
                Live ->
                    ( div [ class "row live", title "www.twitch.tv/bonjwa", onClick (OpenTab "https://www.twitch.tv/bonjwa") ]
                    , div [ class "live-indicator" ]
                        [ span [ class "dot" ] [ text "●" ]
                        , text "live"
                        ]
                    )

                Alarm ->
                    let
                        dialog =
                            { message = "Geplante Erinnerung deaktivieren?"
                            , icon = "../images/ic_alarm_off_white_48px.svg"
                            , positive = { text = "Deaktivieren", msg = DialogMsg ActionClearAlarm }
                            , negative = Just { text = "Abbrechen", msg = DialogMsg ActionCancel }
                            }
                    in
                    ( div [ class "row", title "Erinnerung deaktivieren", onClick (ShowDialog dialog) ]
                    , div [ class "alarm-indicator" ]
                        [ text "alarm"
                        ]
                    )

                NoAlarm ->
                    ( div [ class "row" ]
                    , text ""
                    )

                Default ->
                    let
                        alarmConfig =
                            { timestamp = Time.posixToMillis start
                            , title = "Live-Sendung hat begonnen"
                            , message = timeText ++ "\n" ++ topic ++ "\n\n(Klicken um Twitch zu öffnen)"
                            }

                        dialog =
                            { message = "Erinnerung für den gewählten Programm-Slot aktivieren?"
                            , icon = "../images/ic_alarm_white_48px.svg"
                            , positive = { text = "Aktivieren", msg = DialogMsg (ActionSetAlarm alarmConfig) }
                            , negative = Just { text = "Abbrechen", msg = DialogMsg ActionCancel }
                            }
                    in
                    ( div [ class "row", title "Erinnerung aktivieren", onClick (ShowDialog dialog) ]
                    , text ""
                    )
    in
    rowElement
        [ div [ class "left" ] [ div [ class timeClass ] time ]
        , div [ class "right" ]
            [ indicator
            , div [ class "game" ]
                [ strong [] [ text game ]
                ]
            , div [ class "streamers" ] [ text streamersText ]
            ]
        ]


filterBroadcasts : Time.Zone -> Time.Posix -> Broadcasts -> Broadcasts
filterBroadcasts zone today broadcasts =
    let
        ceilingDate =
            Time.Extra.ceiling Day zone today

        floorDate =
            Time.Extra.floor Day zone today

        isToday =
            isDateBetween zone floorDate ceilingDate
    in
    List.filter (\{ start, end } -> isToday start || isToday end) broadcasts


sortBroadcasts : Time.Zone -> Broadcasts -> Broadcasts
sortBroadcasts zone =
    List.sortWith (\a b -> compareDate zone a.start b.start)


viewBroadcastTime : Time.Zone -> Span -> Broadcast -> List (Html Msg)
viewBroadcastTime zone spanType broadcast =
    let
        timeLeft =
            case spanType of
                Previous ->
                    span [ class "previous-span" ] [ text (formatTime zone broadcast.start) ]

                _ ->
                    span [] [ text (formatTime zone broadcast.start) ]

        timeRight =
            case spanType of
                Next ->
                    span [ class "next-span" ] [ text (formatTime zone broadcast.end) ]

                _ ->
                    span [] [ text (formatTime zone broadcast.end) ]
    in
    [ timeLeft, text " - ", timeRight ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveAlarm (decodeAlarmValue handleAlarmResult)


handleAlarmResult : Result Json.Decode.Error (Maybe Int) -> Msg
handleAlarmResult result =
    case result of
        Ok time ->
            ReceiveAlarm time

        Err err ->
            ReceiveAlarmError err



-- DATE


requestInit : Cmd Msg
requestInit =
    Task.map2 pair Time.here Time.now
        |> Task.perform ReceiveInitialDate


requestDate : Cmd Msg
requestDate =
    Task.map2 pair Time.here Time.now
        |> Task.perform ReceiveDate



-- HTTP


requestBroadcasts : Cmd Msg
requestBroadcasts =
    let
        url =
            "https://bnjw.viceair.com/broadcasts"
    in
    Http.get url broadcastsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map BroadcastResponse
