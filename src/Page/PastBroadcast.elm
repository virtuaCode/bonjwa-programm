module Page.PastBroadcast exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Data.PastBroadcast exposing (PastBroadcast, PastBroadcasts, PastBroadcastsWebData, pastBroadcastsDecoder)
import Date exposing (Date, now)
import Date.Extra exposing (Interval(Day), diff)
import Dom exposing (Error, focus)
import Html exposing (..)
import Html.Attributes exposing (class, id, src, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import RemoteData exposing (RemoteData(..))
import RemoteData.Http exposing (get)
import Task exposing (Task, perform)
import Util exposing ((=>), addDay, addDays, dateEqual, formatDate, formatDuration, srcset)
import Views.Container as Container
import Views.Message as Message


-- MODEL


type alias Model =
    { broadcasts : PastBroadcastsWebData
    , search : Maybe String
    , offset : Int
    , date : Maybe Date
    }


type Style
    = Normal
    | WithDate


type Msg
    = NextDay
    | ClickedBack
    | ClickedLink String
    | InitDate Date
    | PrevDay
    | Response PastBroadcastsWebData
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
    initModel => initDate


initModel : Model
initModel =
    { broadcasts = NotAsked
    , offset = 0
    , search = Nothing
    , date = Nothing
    }


initDate : Cmd Msg
initDate =
    perform InitDate now



-- UPDATE


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        NextDay ->
            { model | offset = model.offset + 1 }
                => Cmd.none
                => NoOp

        PrevDay ->
            { model | offset = model.offset - 1 }
                => Cmd.none
                => NoOp

        InitDate date ->
            { model | date = Just date }
                => requestPastBroadcasts
                => NoOp

        ClickedLink link ->
            model
                => Cmd.none
                => OpenTab link

        ClickedBack ->
            model
                => Cmd.none
                => Back

        Response broadcasts ->
            { model | broadcasts = broadcasts, offset = offsetLatest model.date broadcasts }
                => Cmd.none
                => NoOp

        Search ->
            { model | search = Just "" }
                => focusSearchField
                => NoOp

        CancelSearch ->
            { model | search = Nothing }
                => Cmd.none
                => NoOp

        SearchFocusResult _ ->
            model
                => Cmd.none
                => NoOp

        InputSearch newTerm ->
            case model.search of
                Nothing ->
                    model
                        => Cmd.none
                        => NoOp

                Just _ ->
                    { model | search = Just newTerm }
                        => Cmd.none
                        => NoOp


focusSearchField : Cmd Msg
focusSearchField =
    Task.attempt SearchFocusResult (focus "search-field")



-- VIEW


viewContent : Maybe Date -> Int -> PastBroadcastsWebData -> Html Msg
viewContent date offset remoteData =
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
                    addDays offset currentDate

                visiblePastBroadcasts =
                    broadcasts
                        |> filterPastBroadcasts offsetDate
            in
            case visiblePastBroadcasts of
                [] ->
                    Message.view "Für diesen Tag existieren keine Past Broadcasts."

                _ ->
                    div [ id "table" ] (viewPastBroadcasts Normal visiblePastBroadcasts)


viewContentSearch : String -> PastBroadcastsWebData -> Html Msg
viewContentSearch term remoteData =
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
                        |> sortPastBroadcasts
            in
            case visiblePastBroadcasts of
                [] ->
                    Message.view <| "Zu dem Suchbegriff '" ++ (String.trim term) ++ "' wurden keine passenden Past Broadcasts gefunden."  

                _ ->
                    div [ id "table" ] (viewPastBroadcasts WithDate visiblePastBroadcasts)


view : Model -> Html Msg
view { date, offset, broadcasts, search } =
    let
        buttons =
            [ span [ class "button", onClick Search ]
                [ img [ src "../images/search_48_1x.png", srcset [ "../images/search_48_1x.png", "../images/search_48_2x.png" ] ] []
                ]
            ]

        header =
            [ span [ class "back", onClick ClickedBack ] []
            , span [ class "title" ] [ text "PAST BROADCASTS" ]
            ]

        content =
            case search of
                Nothing ->
                    viewContent date offset broadcasts

                Just term ->
                    viewContentSearch term broadcasts

        dateString =
            date |> Maybe.map (addDays offset >> formatDate) >> Maybe.withDefault ""

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


viewPastBroadcasts : Style -> List PastBroadcast -> List (Html Msg)
viewPastBroadcasts style broadcasts =
    List.map (viewPastBroadcast style) broadcasts


viewPastBroadcast : Style -> PastBroadcast -> Html Msg
viewPastBroadcast style broadcast =
    let
        dateBadge =
            if style == WithDate then
                span [ class "date nowrap" ] [ text (formatDate broadcast.date) ]
            else
                text ""
    in
    div [ class "item", onClick (ClickedLink broadcast.link) ]
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
    , div [ id "cancel", onClick CancelSearch ]
        [ span [ class "cancel" ] []
        ]
    ]


viewNavigation : String -> List (Html Msg)
viewNavigation date =
    [ div [ id "prev", onClick PrevDay ] [ span [ class "prev" ] [] ]
    , div [ id "day" ] [ text date ]
    , div [ id "next", onClick NextDay ] [ span [ class "next" ] [] ]
    ]



-- HELPER


filterPastBroadcasts : Date -> PastBroadcasts -> PastBroadcasts
filterPastBroadcasts today broadcasts =
    List.filter (\{ date } -> dateEqual today date) broadcasts


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

                inGame term =
                    String.contains term lowerGame
            in
            List.all inGame termWords
    in
    List.filter match broadcasts


sortPastBroadcasts : PastBroadcasts -> PastBroadcasts
sortPastBroadcasts =
    List.sortWith (\a b -> Date.Extra.compare b.date a.date)


maximumDate : PastBroadcasts -> Maybe Date
maximumDate broadcasts =
    broadcasts
        |> List.map .date
        |> List.Extra.maximumBy Date.toTime


offsetLatest : Maybe Date -> PastBroadcastsWebData -> Int
offsetLatest maybeDate broadcastsData =
    case ( maybeDate, broadcastsData ) of
        ( Just date, Success broadcasts ) ->
            let
                maybeLatest =
                    maximumDate broadcasts
            
                floorDay =
                    Date.Extra.floor Day
                
            in
            case maybeLatest of
                Just latest ->
                    Date.Extra.diff Day (floorDay date) (floorDay latest)

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
    get url Response pastBroadcastsDecoder
