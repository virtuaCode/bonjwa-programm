module Views.Dialog exposing (..)

import Data.Dialog exposing (Button, Dialog)
import Html exposing (..)
import Html.Attributes exposing (class, id, src, width, height)
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
viewButtons positive negative =
    case negative of
        Nothing ->
            [ span [ class "button positive", onClick positive.msg ] [ text positive.text ] ]

        Just negative ->
            [ span [ class "button negative", onClick negative.msg ] [ text negative.text ]
            , span [ class "space" ] []
            , span [ class "button positive", onClick positive.msg ] [ text positive.text ]
            ]
