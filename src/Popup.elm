module Popup exposing (main)

import Browser exposing (openTab)
import Data.Broadcast exposing (..)
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
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing 0 NotAsked Nothing
    , requestInit
    )



-- BROADCAST


styleBroadcasts : Date -> Broadcasts -> List (Styled Broadcast)
styleBroadcasts time =
    List.map (styleBroadcast time)


styleBroadcast : Date -> Broadcast -> Styled Broadcast
styleBroadcast time broadcast =
    let
        now =
            isTimeBetweenBroadcast time broadcast
    in
    if now then
        Primary broadcast
    else
        Secondary broadcast


isTimeBetweenBroadcast : Date -> Broadcast -> Bool
isTimeBetweenBroadcast time { start, end } =
    Date.Extra.isBetween start end time


type Styled a
    = Primary a
    | Secondary a



-- UPDATE


type Msg
    = NextDay
    | PrevDay
    | ReceiveInitialDate Date
    | ReceiveDate Date
    | ShowRoute Route
    | OpenTab String
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
            --toPage PastBroadcastsPage PastBroadcastMsg Page.PastBroadcast.update subMsg subModel
            --    ( { model | subpage = Page.PastBroadcast.update subMsg subModel }, Cmd.none )
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
    case model.subpage of
        Nothing ->
            viewProgramm model

        Just (PastBroadcastsPage subModel) ->
            Page.PastBroadcast.view subModel
                |> Html.map PastBroadcastMsg



-- Programm


viewProgramm : Model -> Html Msg
viewProgramm model =
    let
        dateString =
            model.date |> Maybe.map (addDays model.offset >> formatDate) >> Maybe.withDefault ""

        content =
            viewProgrammContent model.date model.offset model.broadcasts
    in
    viewProgrammContainer dateString content


viewProgrammContent : Maybe Date -> Int -> BroadcastsWebData -> Html Msg
viewProgrammContent date offset remoteData =
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
                        |> styleBroadcasts currentDate
            in
            viewBroadcastTable visibleBroadcasts


viewProgrammHeader : List (Html Msg)
viewProgrammHeader =
    [ span [ class "button button-emote", title "www.bonjwa.de", onClick (OpenTab "https://www.bonjwa.de") ]
        [ img [ src "../images/chill_28.png" ] []
        ]
    , span [ class "title" ] [ text "PROGRAMM" ]
    , span [ class "button", title "PAST BROADCASTS", onClick (ShowRoute Route.PastBroadcasts) ]
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
        ( rowElement, indicator, { start, end, topic } ) =
            case styledBroadcast of
                Primary broadcast ->
                    ( div [ class "row live", title "www.twitch.tv/bonjwa", onClick (OpenTab "https://www.twitch.tv/bonjwa") ]
                    , div [ class "live-indicator" ]
                        [ span [ class "dot" ] [ text "●" ]
                        , text "live"
                        ]
                    , broadcast
                    )

                Secondary broadcast ->
                    ( div [ class "row" ]
                    , text ""
                    , broadcast
                    )

        time =
            formatTimeRange start end
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
    Sub.none



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
