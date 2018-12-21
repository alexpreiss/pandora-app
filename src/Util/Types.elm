module Util.Types exposing (..)

import Material


type alias GlobalModel =
    { page : Page
    , authToken : String
    , deletingStationPopup : Bool
    , updatingStationPopup : Bool
    , email : String
    , username : String
    , newUser : Bool
    , mdl : Material.Model
    , keyPress : Int
    , seek : Float
    , isPlaying : Bool
    , audioLevel : Float
    , audioHover : Float
    , currentTime : Float
    , songQueue : List Song
    , currentStation : Maybe Station
    , stations : List Station
    , previousSongs : List Song
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
