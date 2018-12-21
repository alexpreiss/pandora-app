module Type.StateTypes exposing (..)


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
