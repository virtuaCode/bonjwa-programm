module Page.PastBroadcast exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Browser.Dom exposing (Error, focus)
import Data.PastBroadcast exposing (PastBroadcast, PastBroadcasts, PastBroadcastsWebData, pastBroadcastsDecoder)
import Html exposing (..)
import Html.Attributes exposing (class, id, src, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (get)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Task exposing (Task, perform)
import Time
import Time.Extra exposing (Interval(..))
import Util exposing (addDay, addDays, compareDate, dateEqual, formatDate, formatDuration, pair, pairLeft, srcset)
import Views.Container as Container
import Views.Message as Message



-- MODEL


type alias Model =
    { broadcasts : PastBroadcastsWebData
    , search : Maybe String
    , offset : Int
    , date : Maybe Time.Posix
    , zone : Time.Zone
    }


type Style
    = Normal
    | WithDate


type Msg
    = NextDay
    | ClickedBack
    | ClickedLink String
    | InitDate Time.Posix
    | PrevDay
    | PastBroadcastsResponse PastBroadcastsWebData
    | Search
    | CancelSearch
    | SearchFocusResult (Result Error ())
    | InputSearch String


type ExternalMsg
    = NoOp
    | Back
    | OpenTab String



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initModel, initDate )


initModel : Model
initModel =
    { broadcasts = NotAsked
    , offset = 0
    , search = Nothing
    , date = Nothing
    , zone = Time.utc
    }


initDate : Cmd Msg
initDate =
    perform InitDate Time.now



-- UPDATE


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NextDay ->
            pairLeft
                { model | offset = model.offset + 1 }
                Cmd.none
                NoOp

        PrevDay ->
            pairLeft
                { model | offset = model.offset - 1 }
                Cmd.none
                NoOp

        InitDate date ->
            pairLeft
                { model | date = Just date }
                requestPastBroadcasts
                NoOp

        ClickedLink link ->
            pairLeft
                model
                Cmd.none
                (OpenTab link)

        ClickedBack ->
            pairLeft
                model
                Cmd.none
                Back

        PastBroadcastsResponse broadcasts ->
            pairLeft
                { model | broadcasts = broadcasts, offset = offsetLatest model.zone model.date broadcasts }
                Cmd.none
                NoOp

        Search ->
            pairLeft
                { model | search = Just "" }
                focusSearchField
                NoOp

        CancelSearch ->
            pairLeft
                { model | search = Nothing }
                Cmd.none
                NoOp

        SearchFocusResult _ ->
            pairLeft
                model
                Cmd.none
                NoOp

        InputSearch newTerm ->
            case model.search of
                Nothing ->
                    pairLeft
                        model
                        Cmd.none
                        NoOp

                Just _ ->
                    pairLeft
                        { model | search = Just newTerm }
                        Cmd.none
                        NoOp


focusSearchField : Cmd Msg
focusSearchField =
    Task.attempt SearchFocusResult (focus "search-field")



-- VIEW


viewContent : Time.Zone -> Maybe Time.Posix -> Int -> PastBroadcastsWebData -> Html Msg
viewContent zone date offset remoteData =
    case ( remoteData, date ) of
        ( NotAsked, _ ) ->
            Message.view "Daten werden geladen..."

        ( Loading, _ ) ->
            Message.view "Daten werden geladen..."

        ( Failure _, _ ) ->
            Message.view "Serveranfrage fehlgeschlagen!"

        ( Success _, Nothing ) ->
            Message.view "Daten werden geladen..."

        ( Success broadcasts, Just currentDate ) ->
            let
                offsetDate =
                    addDays offset zone currentDate

                visiblePastBroadcasts =
                    broadcasts
                        |> filterPastBroadcasts zone offsetDate
            in
            case visiblePastBroadcasts of
                [] ->
                    Message.view "Für diesen Tag existieren keine Past Broadcasts."

                _ ->
                    div [ id "table" ] (viewPastBroadcasts zone Normal visiblePastBroadcasts)


viewContentSearch : Time.Zone -> String -> PastBroadcastsWebData -> Html Msg
viewContentSearch zone term remoteData =
    case remoteData of
        NotAsked ->
            Message.view "Daten werden geladen..."

        Loading ->
            Message.view "Daten werden geladen..."

        Failure _ ->
            Message.view "Serveranfrage fehlgeschlagen!"

        Success broadcasts ->
            let
                visiblePastBroadcasts =
                    broadcasts
                        |> searchPastBroadcasts term
                        |> sortPastBroadcasts zone
            in
            case visiblePastBroadcasts of
                [] ->
                    Message.view <| "Zu dem Suchbegriff '" ++ String.trim term ++ "' wurden keine passenden Past Broadcasts gefunden."

                _ ->
                    div [ id "table" ] (viewPastBroadcasts zone WithDate visiblePastBroadcasts)


view : Model -> Html Msg
view { zone, date, offset, broadcasts, search } =
    let
        buttons =
            [ span [ class "button", title "Past Broadcasts durchsuchen", onClick Search ]
                [ img [ src "../images/search_48_1x.png", srcset [ "../images/search_48_1x.png", "../images/search_48_2x.png" ] ] []
                ]
            ]

        header =
            [ span [ class "back", title "Zurück", onClick ClickedBack ] []
            , span [ class "title" ] [ text "PAST BROADCASTS" ]
            ]

        content =
            case search of
                Nothing ->
                    viewContent zone date offset broadcasts

                Just term ->
                    viewContentSearch zone term broadcasts

        dateString =
            date |> Maybe.map (addDays offset zone >> formatDate zone) >> Maybe.withDefault ""

        navigation =
            case search of
                Nothing ->
                    viewNavigation dateString

                Just term ->
                    viewSearchNavigation term
    in
    Container.view
        "past-broadcasts"
        (header ++ buttons)
        navigation
        content


viewPastBroadcasts : Time.Zone -> Style -> List PastBroadcast -> List (Html Msg)
viewPastBroadcasts zone style broadcasts =
    List.map (viewPastBroadcast zone style) broadcasts


viewPastBroadcast : Time.Zone -> Style -> PastBroadcast -> Html Msg
viewPastBroadcast zone style broadcast =
    let
        dateBadge =
            if style == WithDate then
                span [ class "date nowrap" ] [ text (formatDate zone broadcast.date) ]

            else
                text ""
    in
    div [ class "item", title broadcast.link, onClick (ClickedLink broadcast.link) ]
        [ div [ class "flex top-line" ]
            [ div [ class "game-top game text-ellipsis" ] [ text broadcast.game ]
            , dateBadge
            ]
        , div [ class "flex" ]
            [ div [ class "mods text-ellipsis" ] [ text broadcast.mods ]
            , div [ class "duration nowrap" ] [ text (formatDuration broadcast.duration) ]
            ]
        ]


viewSearchNavigation : String -> List (Html Msg)
viewSearchNavigation term =
    [ div [ class "search-label" ] [ text "GAME:" ]
    , input [ id "search-field", class "search", type_ "text", value term, onInput InputSearch ] []
    , div [ id "cancel", title "Suche beenden", onClick CancelSearch ]
        [ span [ class "cancel" ] []
        ]
    ]


viewNavigation : String -> List (Html Msg)
viewNavigation date =
    [ div [ id "prev", title "Vorheriger Tag", onClick PrevDay ] [ span [ class "prev" ] [] ]
    , div [ id "day" ] [ text date ]
    , div [ id "next", title "Nächster Tag", onClick NextDay ] [ span [ class "next" ] [] ]
    ]



-- HELPER


filterPastBroadcasts : Time.Zone -> Time.Posix -> PastBroadcasts -> PastBroadcasts
filterPastBroadcasts zone today broadcasts =
    List.filter (\{ date } -> dateEqual zone today date) broadcasts


searchPastBroadcasts : String -> PastBroadcasts -> PastBroadcasts
searchPastBroadcasts term broadcasts =
    let
        match { game } =
            let
                lowerTerm =
                    String.toLower term

                lowerGame =
                    String.toLower game

                termWords =
                    String.words lowerTerm

                inGame x =
                    String.contains x lowerGame
            in
            List.all inGame termWords
    in
    List.filter match broadcasts


sortPastBroadcasts : Time.Zone -> PastBroadcasts -> PastBroadcasts
sortPastBroadcasts zone =
    List.sortWith (\a b -> compareDate zone a.date b.date)


maximumDate : PastBroadcasts -> Maybe Time.Posix
maximumDate broadcasts =
    broadcasts
        |> List.map .date
        |> List.Extra.maximumBy Time.posixToMillis


offsetLatest : Time.Zone -> Maybe Time.Posix -> PastBroadcastsWebData -> Int
offsetLatest zone maybeDate broadcastsData =
    case ( maybeDate, broadcastsData ) of
        ( Just date, Success broadcasts ) ->
            let
                maybeLatest =
                    maximumDate broadcasts

                floorDay =
                    Time.Extra.floor Day zone
            in
            case maybeLatest of
                Just latest ->
                    Time.Extra.diff Day zone (floorDay date) (floorDay latest)

                Nothing ->
                    0

        _ ->
            0



-- WEBDATA


requestPastBroadcasts : Cmd Msg
requestPastBroadcasts =
    let
        url =
            "https://bnjw.viceair.com/pastbroadcasts"
    in
    Http.get url pastBroadcastsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map PastBroadcastsResponse
