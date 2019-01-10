module Util.Types exposing (..)

import Material
import Dict exposing (Dict)
import Color.Convert as Convert
import Color


cmds : List (Cmd msg) -> Cmd msg
cmds =
    Cmd.batch


hexToColor :
    String
    ->
        { red : Int
        , green : Int
        , blue : Int
        , alpha : Float
        }
hexToColor color =
    (case Convert.hexToColor color of
        Ok rgb ->
            Color.toRgb rgb

        Err error ->
            { red = 0
            , green = 0
            , blue = 0
            , alpha = 0
            }
    )


colorIsLight : String -> Bool
colorIsLight color =
    let
        listCase item =
            case item of
                Just item ->
                    Debug.log "islight list case item " item

                Nothing ->
                    0

        rgbaRecord =
            hexToColor color

        a =
            1
                - (0.299
                    * toFloat rgbaRecord.red
                    + 0.587
                    * toFloat rgbaRecord.green
                    + 0.114
                    * toFloat rgbaRecord.blue
                  )
                / 255
    in
        a < 0.5


textColor : GlobalModel -> String
textColor gm =
    if
        colorIsLight
            (case List.head gm.songQueue of
                Just song ->
                    Debug.log "dominantColor" song.dominantColor

                Nothing ->
                    "FFFFFF"
            )
    then
        "black"
    else
        "white"


type alias LoginModel =
    { email : String
    , password : String
    , remember : Bool
    , failed : Bool
    }


type alias SelectorModel =
    { state : SelectorState
    , previousSongs : List Song
    , searchResults : Dict String SearchResult
    , searchInput : String
    }


type alias UiModel =
    {}


type SelectorState
    = Selecting
    | Searching


type alias GlobalModel =
    { page : Page
    , authToken : String
    , removingStationPopup : Bool
    , updatingStationPopup : Bool
    , username : Maybe String
    , newUser : Maybe Bool
    , mdl : Material.Model
    , keyPress : Int
    , seek : Float
    , isPlaying : Bool
    , audioLevel : Maybe Float
    , audioHover : Bool
    , currentTime : Float
    , songQueue : List Song
    , currentStation : Maybe Station
    , stations : List Station
    , previousSongs : List Song
    , updateStationNameInput : String
    , error : String
    , dominantColor : String

    -- Page Models
    , loginModel : LoginModel
    , selectorModel : SelectorModel
    , uiModel : UiModel
    }


type Page
    = LoginWindow
    | Player
    | StationSelector
    | ChatWindow
    | PreviousSongs


type alias Chat =
    { id : Int
    , email : String
    , username : String
    , content : String
    }


type alias Song =
    { songTitle : String
    , trackLength : Int
    , rating : Int
    , audioURL : String
    , artistName : String
    , albumTitle : String
    , albumArt : String
    , trackToken : String
    , pandoraId : String
    , dominantColor : String
    }


type alias Station =
    { id : String
    , name : String
    , art : String
    }


type alias SearchResult =
    { pandoraId : String
    , artistName : Maybe String
    , name : Maybe String
    , resultType : String
    , art : Maybe String
    , thorId : Maybe String
    , dominantColor : Maybe String
    }
