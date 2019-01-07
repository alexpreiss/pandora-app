module Util.Types exposing (..)

import Material
import Dict exposing (Dict)


cmds : List (Cmd msg) -> Cmd msg
cmds =
    Cmd.batch


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
    | PreviousSongs


type alias GlobalModel =
    { page : Page
    , authToken : String
    , deletingStationPopup : Bool
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
