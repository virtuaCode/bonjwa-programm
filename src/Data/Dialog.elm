module Data.Dialog exposing (Button, Dialog)


type alias Button msg =
    { text : String
    , msg : msg
    }


type alias Dialog msg =
    { message : String
    , icon : String
    , positive : Button msg
    , negative : Maybe (Button msg)
    }
