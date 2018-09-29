module Views.Dialog exposing (viewButtons, viewDialog)

import Data.Dialog exposing (Button, Dialog)
import Html exposing (..)
import Html.Attributes exposing (class, height, id, src, width)
import Html.Events exposing (onClick)


viewDialog : Dialog msg -> Html msg
viewDialog dialog =
    let
        { message, icon, negative, positive } =
            dialog

        buttons =
            viewButtons positive negative
    in
    div [ id "dialog" ]
        [ h3 []
            [ img [ src icon, width 32, height 32 ] []
            , text message
            ]
        , div [ class "footer" ] buttons
        ]


viewButtons : Button msg -> Maybe (Button msg) -> List (Html msg)
viewButtons positiveButton negative =
    case negative of
        Nothing ->
            [ span [ class "button positive", onClick positiveButton.msg ] [ text positiveButton.text ] ]

        Just negativeButton ->
            [ span [ class "button negative", onClick negativeButton.msg ] [ text negativeButton.text ]
            , span [ class "space" ] []
            , span [ class "button positive", onClick positiveButton.msg ] [ text positiveButton.text ]
            ]
