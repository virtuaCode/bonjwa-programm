module Popup exposing (main)

import Browser exposing (clearAlarm, decodeAlarmValue, getAlarm, openTab, receiveAlarm, setAlarm)
import Data.AlarmConfig exposing (..)
import Data.Broadcast exposing (..)
import Data.Dialog as Dialog exposing (Button, Dialog)
import Date exposing (Date, Day(..))
import Date.Extra exposing (Interval(..), ceiling, floor, isBetween)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.PastBroadcast
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Remote exposing (get)
import Route exposing (Route(..))
import Task
import Util exposing (..)
import Views.Container as Container
import Views.Dialog
import Views.Message as Message


main : Program Never Model Msg
main =
    Html.program
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
    { date : Maybe Date
    , offset : Int
    , broadcasts : BroadcastsWebData
    , subpage : Maybe Page
    , dialog : Maybe (Dialog.Dialog Msg)
    , alarm : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ getAlarm (), requestInit ] )


initModel : Model
initModel =
    { date = Nothing
    , offset = 0
    , broadcasts = NotAsked
    , subpage = Nothing
    , dialog = Nothing
    , alarm = Nothing
    }



-- BROADCAST


styleBroadcasts : Date -> Maybe Float -> Broadcasts -> List (Styled Broadcast)
styleBroadcasts time alarm =
    List.map (styleBroadcast time alarm)


styleBroadcast : Date -> Maybe Float -> Broadcast -> Styled Broadcast
styleBroadcast time alarm broadcast =
    let
        now =
            isTimeBetweenBroadcast time broadcast
    in
    if now then
        Live broadcast
    else
        case alarm of
            Nothing ->
                Default broadcast

            Just time ->
                if time == Date.toTime broadcast.start then
                    Alarm broadcast
                else
                    Default broadcast


isTimeBetweenBroadcast : Date -> Broadcast -> Bool
isTimeBetweenBroadcast time { start, end } =
    Date.Extra.isBetween start end time


type Styled a
    = Alarm a
    | Default a
    | Live a


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
    | ReceiveInitialDate Date
    | ReceiveDate Date
    | ReceiveAlarm (Maybe Float)
    | ReceiveAlarmError String
    | ShowRoute Route
    | OpenTab String
    | GetAlarm
    | SetAlarm AlarmConfig
    | PastBroadcastMsg Page.PastBroadcast.Msg
    | BroadcastResponse BroadcastsWebData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            { model | subpage = Just (toModel newModel) }
                => Cmd.map toMsg newCmd
    in
    case ( msg, model.subpage ) of
        ( NextDay, _ ) ->
            case model.date of
                Nothing ->
                    model
                        => Cmd.none

                Just date ->
                    { model | offset = model.offset + 1 }
                        => Cmd.none

        ( PrevDay, _ ) ->
            case model.date of
                Nothing ->
                    model
                        => Cmd.none

                Just date ->
                    { model | offset = model.offset - 1 }
                        => Cmd.none

        ( OpenTab url, _ ) ->
            model
                => openTab url

        ( GetAlarm, _ ) ->
            model
                => getAlarm ()

        ( SetAlarm config, _ ) ->
            model
                => setAlarm config

        ( ReceiveAlarm alarm, _ ) ->
            { model | alarm = alarm }
                => Cmd.none

        ( ReceiveInitialDate date, _ ) ->
            { model | date = Just date }
                => requestBroadcasts

        ( ReceiveDate date, _ ) ->
            { model | date = Just date }
                => Cmd.none

        ( BroadcastResponse data, _ ) ->
            { model | broadcasts = data }
                => Cmd.none

        ( ShowRoute route, _ ) ->
            showRoute route model

        ( PastBroadcastMsg subMsg, Just (PastBroadcastsPage subModel) ) ->
            let
                ( ( pageModel, cmd ), msgFromPage ) =
                    Page.PastBroadcast.update subMsg subModel

                ( newModel, command ) =
                    case msgFromPage of
                        Page.PastBroadcast.NoOp ->
                            { model | subpage = Just (PastBroadcastsPage pageModel) }
                                => Cmd.map PastBroadcastMsg cmd

                        Page.PastBroadcast.OpenTab link ->
                            { model | subpage = Just (PastBroadcastsPage pageModel) }
                                => Cmd.batch
                                    [ openTab link
                                    , Cmd.map PastBroadcastMsg cmd
                                    ]

                        Page.PastBroadcast.Back ->
                            { model | subpage = Nothing }
                                => Cmd.map PastBroadcastMsg cmd
            in
            newModel
                => command

        ( ShowDialog dialog, _ ) ->
            { model | dialog = Just dialog }
                => Cmd.none

        ( DialogMsg dialogMsg, _ ) ->
            let
                newModel =
                    { model | dialog = Nothing }
            in
            case dialogMsg of
                ActionCancel ->
                    newModel
                        => Cmd.none

                ActionSetAlarm config ->
                    newModel
                        => setAlarm config

                ActionClearAlarm ->
                    newModel
                        => clearAlarm ()

        ( _, _ ) ->
            model
                => Cmd.none


showRoute : Route -> Model -> ( Model, Cmd Msg )
showRoute route model =
    case route of
        Route.PastBroadcasts ->
            let
                ( pageModel, cmd ) =
                    Page.PastBroadcast.init
            in
            { model | subpage = Just (PastBroadcastsPage pageModel) }
                => Cmd.map PastBroadcastMsg cmd



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
            model.date |> Maybe.map (addDays model.offset >> formatDate) >> Maybe.withDefault ""

        content =
            viewProgrammContent model.date model.offset model.alarm model.broadcasts
    in
    viewProgrammContainer dateString content


viewProgrammContent : Maybe Date -> Int -> Maybe Float -> BroadcastsWebData -> Html Msg
viewProgrammContent date offset alarm remoteData =
    case ( remoteData, date ) of
        ( NotAsked, _ ) ->
            Message.view "Programm wird geladen..."

        ( Loading, _ ) ->
            Message.view "Serveranfrage fehlgeschlagen!"

        ( Failure _, _ ) ->
            Message.view "Serveranfrage fehlgeschlagen!"

        ( Success _, Nothing ) ->
            Message.view "Programm wird geladen..."

        ( Success broadcasts, Just currentDate ) ->
            let
                offsetDate =
                    addDays offset currentDate

                visibleBroadcasts =
                    broadcasts
                        |> filterBroadcasts offsetDate
                        |> sortBroadcasts
                        |> styleBroadcasts currentDate alarm
            in
            viewBroadcastTable visibleBroadcasts


viewProgrammHeader : List (Html Msg)
viewProgrammHeader =
    [ img [ class "bonjwa-logo", src "../images/bonjwa.jpg", title "www.bonjwa.de", onClick (OpenTab "https://www.bonjwa.de"), alt "Bonjwa Logo" ] []
    , span [ class "title" ] [ text "BONJWA PROGRAMM" ]
    , span [ class "button", title "Past Broadcasts", onClick (ShowRoute Route.PastBroadcasts) ]
        [ img [ src "../images/video_48_1x.png", srcset [ "../images/video_48_1x.png", "../images/video_48_2x.png" ] ] []
        ]
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


viewBroadcastTable : List (Styled Broadcast) -> Html Msg
viewBroadcastTable broadcasts =
    let
        rows =
            List.map viewBroadcastRow broadcasts
    in
    div [ id "table" ] rows


viewBroadcastRow : Styled Broadcast -> Html Msg
viewBroadcastRow styledBroadcast =
    let
        { start, end, topic } =
            case styledBroadcast of
                Alarm broadcast ->
                    broadcast

                Live broadcast ->
                    broadcast

                Default broadcast ->
                    broadcast

        time =
            formatTimeRange start end

        ( rowElement, indicator ) =
            case styledBroadcast of
                Live broadcast ->
                    ( div [ class "row live", title "www.twitch.tv/bonjwa", onClick (OpenTab "https://www.twitch.tv/bonjwa") ]
                    , div [ class "live-indicator" ]
                        [ span [ class "dot" ] [ text "●" ]
                        , text "live"
                        ]
                    )

                Alarm broadcast ->
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

                Default broadcast ->
                    let
                        alarmConfig =
                            { timestamp = Date.toTime start
                            , title = "Live-Sendung hat begonnen"
                            , message = time ++ "\n" ++ topic ++ "\n\n(Klicken um Twitch zu öffnen)"
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
        [ div [ class "left" ] [ div [ class "time" ] [ text time ] ]
        , div [ class "right" ]
            [ indicator
            , div [ class "topic" ] [ text topic ]
            ]
        ]


filterBroadcasts : Date -> Broadcasts -> Broadcasts
filterBroadcasts today broadcasts =
    let
        ceilingDate =
            Date.Extra.ceiling Day today

        floorDate =
            Date.Extra.floor Day today

        isToday =
            Date.Extra.isBetween floorDate ceilingDate
    in
    List.filter (\{ start, end } -> isToday start && isToday end) broadcasts


sortBroadcasts : Broadcasts -> Broadcasts
sortBroadcasts =
    List.sortWith (\a b -> Date.Extra.compare a.start b.start)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveAlarm (decodeAlarmValue handleAlarmResult)


handleAlarmResult : Result String (Maybe Float) -> Msg
handleAlarmResult result =
    case result of
        Ok time ->
            ReceiveAlarm time

        Err err ->
            ReceiveAlarmError err



-- DATE


requestInit : Cmd Msg
requestInit =
    Task.perform ReceiveInitialDate Date.now


requestDate : Cmd Msg
requestDate =
    Task.perform ReceiveDate Date.now



-- HTTP


requestBroadcasts : Cmd Msg
requestBroadcasts =
    let
        url =
            "https://bnjw.viceair.com/broadcasts"
    in
    Remote.get url BroadcastResponse broadcastsDecoder
