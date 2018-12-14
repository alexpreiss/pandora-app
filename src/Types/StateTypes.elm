module Types.StateTypes exposing (..)


type alias Station =
    { id : String
    , name : String
    , art : String
    }


type alias AddingStationFields =
    { searchedSongs : Dict String SearchResult
    , searchInput : String
    }


type PlayingState
    = Normal
    | SelectingStation
    | SelectingPlaying
    | AddingStation AddingStationFields
    | PreviousSongs
    | Chatting
        { email : String
        , chatInput : String
        , chats : List Chat
        , username : String
        , newUser : Bool
        , userNameInput : String
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


type alias PlayingFields =
    { authToken : String
    , stations : List Station
    , currentStation : Maybe Station
    , songQueue : List Song
    , currentTime : Float
    , previousSongs : List Song
    , audioLevel : Float
    , audioHover : Bool
    , isPlaying : Bool
    , playingState : PlayingState
    , seek : Float
    , email : String
    , username : String
    , newUser : Bool
    , deletingStationPopup : Bool
    , updatingStationPopup : Bool
    , updateStationNameInput : String
    }


type alias LoggingInFields =
    { email : String
    , password : String
    , remember : Bool
    , failed : Bool
    , audioLevel : Maybe Float
    , username : Maybe String
    , newUser : Maybe Bool
    }
