module Views.Message exposing (view)

import Html exposing (..)
import Html.Attributes exposing (id)


view : String -> Html msg
view message =
    div [ id "status" ] [ text message ]
