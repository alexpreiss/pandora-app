port module Util.Port exposing (..)


type alias Chat =
    { id : Int
    , email : String
    , username : String
    , content : String
    }


port togglePause : () -> Cmd msg


port replaySong : () -> Cmd msg


port audioLevel : Float -> Cmd msg


port logOutLocalStorage : () -> Cmd msg


port rememberEmail : String -> Cmd msg


port sendNewTime : Float -> Cmd msg


port rememberUsername : String -> Cmd msg


port rememberPassword : String -> Cmd msg


port newUser : () -> Cmd msg


port getProgressBarWidth : () -> Cmd msg


port sendProgressBarWidth : (Float -> msg) -> Sub msg


port chatSocket : (Chat -> msg) -> Sub msg
