module Views.Container exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, id)


view : String -> List (Html msg) -> List (Html msg) -> Html msg -> Html msg
view containerId headerContent navContent mainContent =
    div [ class "container", id containerId ]
        [ div [ class "header" ] headerContent
        , div [ class "nav" ] navContent
        , mainContent
        ]
