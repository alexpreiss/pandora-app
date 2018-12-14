port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Http exposing (Request)
import Material.Slider as Slider
import Material.Spinner as Spinner
import Material.Options as Options
import Material.Button as Button
import Material.Menu as Menu
import Material.Icon as Icon
import Material.Grid as Grid exposing (Device(..))
import Material.Tooltip as Tooltip
import Material
import Time
import Types.Feedback as Feedback
import Types.Station as Station exposing (Station, SearchResult)
import Types.Fragment as Fragment exposing (Song)
import Types.Chat as Chat exposing (..)
import Keyboard
import Dict exposing (Dict)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- ΩΩΩ MODEL ΩΩΩ


currentSong : Model -> Song
currentSong model =
    case model.state of
        LoggingIn fields ->
            { songTitle = ""
            , trackLength = 0
            , rating = 0
            , audioURL = ""
            , artistName = ""
            , albumTitle = ""
            , albumArt = ""
            , trackToken = ""
            }

        Playing fields ->
            case (List.head fields.songQueue) of
                Just song ->
                    song

                Nothing ->
                    { songTitle = ""
                    , trackLength = 0
                    , rating = 0
                    , audioURL = ""
                    , artistName = ""
                    , albumTitle = ""
                    , albumArt = ""
                    , trackToken = ""
                    }


type PlayingState
    = Normal
    | SelectingStation
    | SelectingPlaying
    | AddingStation
        { searchedSongs : Dict String SearchResult
        , searchInput : String
        }
    | PreviousSongs
    | Chatting
        { email : String
        , chatInput : String
        , chats : List Chat
        , username : String
        , newUser : Bool
        , userNameInput : String
        }


type State
    = LoggingIn
        { email : String
        , password : String
        , remember : Bool
        , failed : Bool
        , audioLevel : Maybe Float
        , username : Maybe String
        , newUser : Maybe Bool
        }
    | Playing
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
        }


stateCase : Model -> a -> a -> a -> a
stateCase model playing loggingIn failed =
    case model.state of
        Playing fields ->
            playing

        LoggingIn fields ->
            loggingIn


getCurrentStation : Model -> Station
getCurrentStation model =
    case model.state of
        LoggingIn fields ->
            { id = ""
            , name = "Select a station"
            , art = ""
            }

        Playing fields ->
            case fields.currentStation of
                Just station ->
                    station

                Nothing ->
                    { id = ""
                    , name = "Select a station"
                    , art = ""
                    }


type alias Model =
    { state : State
    , mdl : Material.Model
    , getStationError : Bool
    , startStationError : Bool
    , loadingSongError : Bool
    , keyPress : Int
    }


type alias Flags =
    { password : Maybe String
    , audioLevel : Maybe Float
    , username : Maybe String
    , email : Maybe String
    , newUser : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case flags.password of
        Just password ->
            { state =
                LoggingIn
                    { email =
                        (case flags.email of
                            Just email ->
                                email

                            Nothing ->
                                ""
                        )
                    , password = password
                    , remember = True
                    , failed = False
                    , audioLevel = flags.audioLevel
                    , username = flags.username
                    , newUser =
                        if flags.newUser == Just "false" then
                            Just False
                        else
                            Just True
                    }
            , mdl = Material.model
            , getStationError = False
            , startStationError = False
            , loadingSongError = False
            , keyPress = 0
            }
                ! [ Http.send LoggedInRemember
                        (login
                            { email =
                                (case flags.email of
                                    Just email ->
                                        email

                                    Nothing ->
                                        ""
                                )
                            , password = password
                            }
                        )
                  ]

        Nothing ->
            { state =
                LoggingIn
                    { email = ""
                    , password = ""
                    , remember = False
                    , failed = False
                    , audioLevel = flags.audioLevel
                    , username = Nothing
                    , newUser = Nothing
                    }
            , mdl = Material.model
            , getStationError = False
            , startStationError = False
            , loadingSongError = False
            , keyPress = 0
            }
                ! []



-- ΩΩΩ UPDATE ΩΩΩ


authDecode : Decode.Decoder String
authDecode =
    (Decode.at [ "authToken" ] Decode.string)


login :
    { email : String
    , password : String
    }
    -> Request String
login info =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            ]
        , url = "https://www.pandora.com/api/v1/auth/login"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "username", (Encode.string info.email) )
                    , ( "password", (Encode.string info.password) )
                    , ( "existingAuthToken", Encode.null )
                    , ( "keepLoggedIn", (Encode.bool True) )
                    ]
                )
        , expect = Http.expectJson authDecode
        , timeout = Nothing
        , withCredentials = False
        }


type
    Msg
    -- Auth
    = InputEmail String
    | InputPassword String
    | Login
    | Logout
    | LoginRemember
    | RememberMe Bool
    | LoggedIn (Result Http.Error String)
    | LoggedInRemember (Result Http.Error String)
    | GotStations (Result Http.Error (List Station))
      -- Controls
    | StartStation String String String
    | StartedStation (Result Http.Error (List Song))
    | TogglePause
    | ReplaySong
    | SetCurrentTime Float
    | SetSeekLocation Float
    | SetNewTime Float
    | SongEnded String
    | SkipSong String
    | LoadNextSongs String
    | LoadedNextSongs (Result Http.Error (List Song))
      --  Audio
    | SetAudio Float
    | Mute
    | UnMute
    | HoveringAudio
    | UnHoveringAudio
      -- Navigation
    | ToStations
    | ToPlaying
    | ToPreviousSongs
    | ToChat
    | ToSongSearch
    | PlayPreviousSong Song
      -- Feedback
    | SendThumbsDown String
    | SendThumbsUp String
    | RemoveThumbsUp String
    | SentFeedBack (Result Http.Error String)
    | RemovedFeedBack (Result Http.Error String)
      -- Misc
    | Mdl (Material.Msg Msg)
    | NoOp
    | NoOp1 String
    | KeyDown Int
      -- Chatting
    | ChatInput String
    | UserNameInput String
    | SetUserName
    | GotChats (Result Http.Error (List Chat))
    | SendChat
    | SentChat (Result Http.Error String)
    | ChatSocket Chat
      -- Creating Stations
    | SearchForSong String
    | GotSearchedSongs (Result Http.Error (Dict String SearchResult))
    | CreateStation String
    | CreatedStation (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail input ->
            case model.state of
                LoggingIn fields ->
                    { model
                        | state =
                            LoggingIn { fields | email = input }
                    }
                        ! []

                Playing fields ->
                    model ! []

        InputPassword input ->
            case model.state of
                LoggingIn fields ->
                    { model
                        | state =
                            LoggingIn { fields | password = input }
                    }
                        ! []

                Playing fields ->
                    model ! []

        Login ->
            case model.state of
                LoggingIn fields ->
                    model
                        ! [ Http.send LoggedIn
                                (login
                                    { email = fields.email
                                    , password = fields.password
                                    }
                                )
                          ]

                Playing fields ->
                    model ! []

        LoginRemember ->
            case model.state of
                LoggingIn fields ->
                    model
                        ! [ Http.send LoggedInRemember
                                (login
                                    { email = fields.email
                                    , password = fields.password
                                    }
                                )
                          , rememberEmail fields.email
                          , rememberPassword fields.password
                          ]

                Playing fields ->
                    model ! []

        LoggedInRemember result ->
            case model.state of
                LoggingIn fields ->
                    case result of
                        Ok token ->
                            { model
                                | state =
                                    Playing
                                        { authToken = token
                                        , stations = []
                                        , currentStation = Nothing
                                        , songQueue = []
                                        , currentTime = 0.0
                                        , previousSongs = []
                                        , audioLevel =
                                            case fields.audioLevel of
                                                Just audio ->
                                                    audio

                                                Nothing ->
                                                    1.0
                                        , audioHover = False
                                        , isPlaying = False
                                        , playingState = SelectingStation
                                        , seek = 0
                                        , email = fields.email
                                        , username =
                                            case fields.username of
                                                Just username ->
                                                    username

                                                Nothing ->
                                                    ""
                                        , newUser =
                                            case fields.newUser of
                                                Just bool ->
                                                    bool

                                                Nothing ->
                                                    True
                                        }
                            }
                                ! [ Http.send GotStations
                                        (Station.get token)
                                  , rememberPassword fields.password
                                  , rememberEmail fields.email
                                  ]

                        Err error ->
                            { model | state = LoggingIn { fields | failed = True } } ! []

                Playing fields ->
                    model ! []

        LoggedIn result ->
            case model.state of
                LoggingIn fields ->
                    case result of
                        Ok token ->
                            { model
                                | state =
                                    Playing
                                        { authToken = Debug.log "Token" token
                                        , stations = []
                                        , currentStation = Nothing
                                        , songQueue = []
                                        , currentTime = 0.0
                                        , previousSongs = []
                                        , audioLevel =
                                            case fields.audioLevel of
                                                Just audio ->
                                                    audio

                                                Nothing ->
                                                    1.0
                                        , audioHover = False
                                        , isPlaying = False
                                        , playingState = SelectingStation
                                        , seek = 0
                                        , email = fields.email
                                        , username =
                                            case fields.username of
                                                Just username ->
                                                    username

                                                Nothing ->
                                                    ""
                                        , newUser =
                                            case fields.newUser of
                                                Just bool ->
                                                    bool

                                                Nothing ->
                                                    True
                                        }
                            }
                                ! [ Http.send
                                        GotStations
                                        (Station.get token)
                                  ]

                        Err error ->
                            { model | state = LoggingIn { fields | failed = True } } ! []

                Playing fields ->
                    model ! []

        GotStations result ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    case result of
                        Ok stations ->
                            { model
                                | state =
                                    Playing { fields | stations = stations }
                            }
                                ! []

                        Err error ->
                            { model | getStationError = True } ! []

        StartStation id name art ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing
                                { fields
                                    | currentStation =
                                        Just
                                            { id = id
                                            , name = name
                                            , art = art
                                            }
                                    , songQueue = []
                                    , playingState = Normal
                                }
                    }
                        ! [ Http.send StartedStation
                                (Fragment.getNext id fields.authToken True)
                          ]

        LoadNextSongs id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    model
                        ! [ Http.send LoadedNextSongs
                                (Fragment.getNext id fields.authToken False)
                          ]

        StartedStation result ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    case result of
                        Ok songs ->
                            { model
                                | state =
                                    Playing
                                        { fields
                                            | songQueue =
                                                List.append
                                                    fields.songQueue
                                                    songs
                                            , currentTime = 0
                                            , isPlaying = True
                                        }
                            }
                                ! []

                        Err error ->
                            { model | startStationError = True }
                                ! []

        LoadedNextSongs result ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    case result of
                        Ok songs ->
                            { model
                                | state =
                                    Playing
                                        { fields
                                            | songQueue =
                                                List.append
                                                    fields.songQueue
                                                    songs
                                        }
                            }
                                ! []

                        Err error ->
                            { model | loadingSongError = True } ! []

        SongEnded id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing
                                { fields
                                    | songQueue = (List.drop 1 fields.songQueue)
                                    , previousSongs =
                                        currentSong model :: fields.previousSongs
                                    , currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ if List.length fields.songQueue == 2 then
                                Http.send LoadedNextSongs
                                    (Fragment.getNext id fields.authToken False)
                            else
                                Cmd.none
                          ]

        SkipSong id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing
                                { fields
                                    | songQueue = (List.drop 1 fields.songQueue)
                                    , previousSongs =
                                        currentSong model :: fields.previousSongs
                                    , currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ if List.length fields.songQueue == 2 then
                                Http.send LoadedNextSongs
                                    (Fragment.getNext id fields.authToken False)
                            else
                                Cmd.none
                          ]

        SetCurrentTime _ ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | currentTime = fields.currentTime + 1 }
                    }
                        ! []

        TogglePause ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | isPlaying = not fields.isPlaying }
                    }
                        ! [ togglePause () ]

        ReplaySong ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing
                                { fields
                                    | currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ replaySong () ]

        SendThumbsDown id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing
                                { fields
                                    | songQueue = (List.drop 1 fields.songQueue)
                                    , previousSongs =
                                        currentSong model :: fields.previousSongs
                                    , currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ Http.send SentFeedBack
                                (Feedback.send fields.authToken
                                    (currentSong model).trackToken
                                    False
                                )
                          , (if List.length fields.songQueue == 2 then
                                Http.send LoadedNextSongs
                                    (Fragment.getNext id fields.authToken False)
                             else
                                Cmd.none
                            )
                          ]

        RemoveThumbsUp id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    let
                        song =
                            List.head fields.songQueue

                        updatedSong =
                            case song of
                                Just song ->
                                    Just { song | rating = 0 }

                                Nothing ->
                                    Nothing

                        updatedModel =
                            { model
                                | state =
                                    Playing
                                        { fields
                                            | songQueue =
                                                case updatedSong of
                                                    Just song ->
                                                        song
                                                            :: (List.drop 1 fields.songQueue)

                                                    Nothing ->
                                                        fields.songQueue
                                        }
                            }
                    in
                        updatedModel
                            ! [ (case (List.head fields.songQueue) of
                                    Just song ->
                                        Http.send RemovedFeedBack
                                            (Feedback.remove
                                                fields.authToken
                                                song.trackToken
                                            )

                                    Nothing ->
                                        Cmd.none
                                )
                              ]

        SendThumbsUp id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    let
                        song =
                            List.head fields.songQueue

                        updatedSong =
                            case song of
                                Just song ->
                                    Just { song | rating = 1 }

                                Nothing ->
                                    Nothing

                        updatedModel =
                            { model
                                | state =
                                    Playing
                                        { fields
                                            | songQueue =
                                                case updatedSong of
                                                    Just song ->
                                                        song
                                                            :: (List.drop
                                                                    1
                                                                    fields.songQueue
                                                               )

                                                    Nothing ->
                                                        fields.songQueue
                                        }
                            }
                    in
                        updatedModel
                            ! [ (case (List.head fields.songQueue) of
                                    Just song ->
                                        Http.send SentFeedBack
                                            (Feedback.send
                                                fields.authToken
                                                song.trackToken
                                                True
                                            )

                                    Nothing ->
                                        Cmd.none
                                )
                              ]

        RemovedFeedBack result ->
            model ! []

        SentFeedBack _ ->
            model ! []

        SetAudio level ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | audioLevel = level }
                    }
                        ! [ audioLevel level ]

        HoveringAudio ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | audioHover = True }
                    }
                        ! []

        UnHoveringAudio ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | audioHover = False }
                    }
                        ! []

        ToStations ->
            case model.state of
                LoggingIn fields ->
                    { model
                        | getStationError = False
                        , loadingSongError = False
                        , startStationError = False
                    }
                        ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | playingState = SelectingPlaying }
                        , getStationError = False
                        , loadingSongError = False
                        , startStationError = False
                    }
                        ! []

        Mdl msg ->
            Material.update Mdl msg model

        NoOp ->
            model ! []

        ToPlaying ->
            case model.state of
                LoggingIn fields ->
                    { model
                        | getStationError = False
                        , loadingSongError = False
                        , startStationError = False
                    }
                        ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | playingState = Normal }
                        , getStationError = False
                        , loadingSongError = False
                        , startStationError = False
                    }
                        ! []

        Mute ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | audioLevel = 0 }
                    }
                        ! [ audioLevel 0 ]

        UnMute ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | audioLevel = 1 }
                    }
                        ! [ audioLevel 1 ]

        Logout ->
            case model.state of
                LoggingIn record ->
                    { model
                        | state =
                            LoggingIn
                                { email = ""
                                , password = ""
                                , remember = False
                                , failed = False
                                , audioLevel = record.audioLevel
                                , username = Nothing
                                , newUser = Just True
                                }
                    }
                        ! []

                Playing record ->
                    { model
                        | state =
                            LoggingIn
                                { email = ""
                                , password = ""
                                , remember = False
                                , failed = False
                                , audioLevel = Just record.audioLevel
                                , username = Nothing
                                , newUser = Just True
                                }
                    }
                        ! []

        RememberMe val ->
            case model.state of
                LoggingIn record ->
                    { model
                        | state =
                            LoggingIn { record | remember = val }
                    }
                        ! []

                Playing record ->
                    model ! []

        SetSeekLocation coord ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model
                        | state =
                            Playing { record | seek = coord }
                    }
                        ! [ getProgressBarWidth () ]

        SetNewTime totalWidth ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    let
                        newTime =
                            (record.seek
                                / totalWidth
                            )
                                * (toFloat (currentSong model).trackLength)
                    in
                        { model
                            | state =
                                Playing { record | currentTime = newTime }
                        }
                            ! [ sendNewTime newTime ]

        ToPreviousSongs ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | playingState = PreviousSongs } } ! []

        ToChat ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    if Debug.log "bool" record.newUser == False then
                        { model
                            | state =
                                Playing
                                    { record
                                        | playingState =
                                            Chatting
                                                { email = record.email
                                                , chatInput = ""
                                                , chats = []
                                                , username = record.username
                                                , newUser = False
                                                , userNameInput = ""
                                                }
                                    }
                        }
                            ! [ Http.send GotChats (Chat.getAll record.authToken)
                              , rememberUsername record.username
                              , newUser ()
                              ]
                    else
                        { model
                            | state =
                                Playing
                                    { record
                                        | playingState =
                                            Chatting
                                                { email = record.email
                                                , chatInput = ""
                                                , chats = []
                                                , username = ""
                                                , newUser = True
                                                , userNameInput = ""
                                                }
                                    }
                        }
                            ! [ Http.send GotChats (Chat.getAll record.authToken)
                              ]

        PlayPreviousSong song ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model
                        | state =
                            Playing
                                { record
                                    | songQueue = song :: record.songQueue
                                    , currentTime = 0
                                    , isPlaying = True
                                    , previousSongs = List.filter (\songTest -> not (songTest.trackToken == song.trackToken)) record.previousSongs
                                    , playingState = Normal
                                }
                    }
                        ! []

        ChatInput content ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case record.playingState of
                        Normal ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        AddingStation fields ->
                            model ! []

                        Chatting fields ->
                            { model
                                | state =
                                    Playing
                                        { record | playingState = Chatting { fields | chatInput = content } }
                            }
                                ! []

        GotChats result ->
            case result of
                Ok chatList ->
                    case model.state of
                        LoggingIn record ->
                            model ! []

                        Playing record ->
                            case record.playingState of
                                Normal ->
                                    model ! []

                                SelectingStation ->
                                    model ! []

                                SelectingPlaying ->
                                    model ! []

                                PreviousSongs ->
                                    model ! []

                                AddingStation fields ->
                                    model ! []

                                Chatting fields ->
                                    { model
                                        | state =
                                            Playing
                                                { record | playingState = Chatting { fields | chats = chatList } }
                                    }
                                        ! []

                Err _ ->
                    model ! []

        KeyDown code ->
            case model.state of
                LoggingIn record ->
                    model
                        ! if code == 13 then
                            (if record.remember then
                                [ Http.send LoggedInRemember
                                    (login
                                        { email = record.email
                                        , password = record.password
                                        }
                                    )
                                , rememberEmail record.email
                                , rememberPassword record.password
                                ]
                             else
                                [ Http.send LoggedIn
                                    (login
                                        { email = record.email
                                        , password = record.password
                                        }
                                    )
                                ]
                            )
                          else
                            [ Cmd.none ]

                Playing record ->
                    (if code == 32 then
                        { model
                            | keyPress = code
                            , state =
                                Playing
                                    { record | isPlaying = not record.isPlaying }
                        }
                     else
                        { model
                            | state =
                                Playing
                                    { record | isPlaying = record.isPlaying }
                        }
                    )
                        ! [ (if code == 32 then
                                togglePause ()
                             else
                                Cmd.none
                            )
                          ]

        UserNameInput username ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case record.playingState of
                        Normal ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        AddingStation fields ->
                            model ! []

                        Chatting fields ->
                            { model
                                | state =
                                    Playing
                                        { record
                                            | playingState =
                                                Chatting
                                                    { fields | userNameInput = username }
                                        }
                            }
                                ! []

        SetUserName ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case record.playingState of
                        Normal ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        AddingStation fields ->
                            model ! []

                        Chatting fields ->
                            { model
                                | state =
                                    Playing
                                        { record
                                            | playingState =
                                                Chatting
                                                    { fields
                                                        | username = fields.userNameInput
                                                        , newUser = False
                                                    }
                                        }
                            }
                                ! [ rememberUsername fields.userNameInput
                                  , newUser ()
                                  ]

        SendChat ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case Debug.log "state" record.playingState of
                        Normal ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        AddingStation fields ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        Chatting fields ->
                            { model
                                | state =
                                    Playing
                                        { record
                                            | playingState =
                                                Chatting
                                                    { fields | chatInput = "" }
                                        }
                            }
                                ! [ Http.send SentChat
                                        (Chat.send
                                            { email = Debug.log "email" fields.email
                                            , username =
                                                record.username
                                            , content = fields.chatInput
                                            }
                                        )
                                  ]

        SentChat result ->
            model ! []

        ChatSocket message ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case record.playingState of
                        Normal ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        AddingStation fields ->
                            model ! []

                        Chatting fields ->
                            { model
                                | state =
                                    Playing
                                        { record
                                            | playingState = Chatting { fields | chats = message :: fields.chats }
                                        }
                            }
                                ! []

        NoOp1 _ ->
            model ! []

        ToSongSearch ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model
                        | state =
                            Playing
                                { record
                                    | playingState =
                                        AddingStation
                                            { searchedSongs = Dict.empty
                                            , searchInput = ""
                                            }
                                }
                    }
                        ! []

        SearchForSong input ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    model ! [ Http.send GotSearchedSongs (Station.search input record.authToken) ]

        GotSearchedSongs result ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    case record.playingState of
                        Normal ->
                            model ! []

                        SelectingPlaying ->
                            model ! []

                        SelectingStation ->
                            model ! []

                        PreviousSongs ->
                            model ! []

                        AddingStation fields ->
                            case Debug.log "songs" result of
                                Ok songs ->
                                    { model
                                        | state =
                                            Playing
                                                { record
                                                    | playingState =
                                                        AddingStation
                                                            { fields
                                                                | searchedSongs = songs
                                                                , searchInput = fields.searchInput
                                                            }
                                                }
                                    }
                                        ! []

                                Err error ->
                                    let
                                        log =
                                            Debug.log "Error searching songs" error
                                    in
                                        model ! []

                        Chatting fields ->
                            model ! []

        CreateStation musicToken ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    model ! [ Http.send CreatedStation (Station.create musicToken record.authToken) ]

        CreatedStation _ ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | playingState = SelectingStation } } ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        LoggingIn fields ->
            Sub.batch [ Keyboard.downs KeyDown ]

        Playing fields ->
            Sub.batch
                [ if
                    fields.isPlaying
                        && not
                            (model.getStationError
                                || model.loadingSongError
                                || model.startStationError
                            )
                  then
                    Time.every Time.second SetCurrentTime
                  else
                    Sub.none
                , Material.subscriptions Mdl model
                , sendProgressBarWidth SetNewTime
                , Keyboard.downs KeyDown
                , chatSocket ChatSocket
                ]



-- ΩΩΩ VIEW ΩΩΩ


onEnded : String -> Attribute Msg
onEnded stationId =
    Html.Events.on "ended"
        (Decode.succeed (SongEnded stationId))


onClickXVal : Attribute Msg
onClickXVal =
    Html.Events.on "click"
        (Decode.map
            SetSeekLocation
            (Decode.field "clientX" Decode.float)
        )


onHover : Attribute Msg
onHover =
    Html.Events.on
        "mouseover"
        (Decode.succeed HoveringAudio)


onUnHover : Attribute Msg
onUnHover =
    Html.Events.on
        "mouseout"
        (Decode.succeed UnHoveringAudio)


audioSlider : Float -> String -> Html Msg
audioSlider modelAudioLevel visibleVal =
    Slider.view
        [ Slider.onChange SetAudio
        , Slider.max 1
        , Slider.min 0
        , Slider.step 0.1
        , Slider.value modelAudioLevel
        , Options.css "width" "100px"
        , Options.css "padding-top" "8px"
        , Options.css "z-index" "2"
        , Options.css "margin-left" "0px"
        , Options.css "visibility" visibleVal
        ]


viewTopMenu :
    Material.Model
    -> PlayingState
    -> List Song
    -> List Song
    -> Html Msg
viewTopMenu modelMdl playingState previousSongs songQueue =
    div
        [ style
            [ ( "margin-left", "auto" )
            , ( "-webkit-user-select", "none" )
            ]
        ]
        [ let
            i name =
                Icon.view name
                    [ Options.css "width" "30px"
                    , Options.css "padding-right" "20px"
                    ]

            onSelect =
                Menu.onSelect
          in
            Menu.render Mdl
                [ 2 ]
                modelMdl
                [ Menu.bottomRight, Menu.icon "keyboard_arrow_down" ]
                (case playingState of
                    Normal ->
                        [ Menu.item
                            [ onSelect ToStations
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ onSelect ToChat
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "chat_bubble", text "Chat" ]
                        , Menu.item
                            [ onSelect ToPreviousSongs
                            , (if previousSongs == [] then
                                Menu.disabled
                               else
                                Options.cs "topMenuIcon"
                              )
                            , Menu.divider
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    SelectingPlaying ->
                        [ Menu.item
                            [ onSelect ToPlaying
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ onSelect ToChat
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "chat_bubble", text "Chat" ]
                        , Menu.item
                            [ onSelect ToPreviousSongs
                            , Menu.divider
                            , (if previousSongs == [] then
                                Menu.disabled
                               else
                                Options.cs "topMenuIcon"
                              )
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    SelectingStation ->
                        [ Menu.item
                            [ onSelect ToPlaying
                            , Menu.disabled
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ onSelect ToChat
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "chat_bubble", text "Chat" ]
                        , Menu.item
                            [ onSelect ToPreviousSongs
                            , Menu.disabled
                            , Menu.divider
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    PreviousSongs ->
                        [ Menu.item
                            [ onSelect ToPlaying
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ onSelect ToChat
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "chat_bubble", text "Chat" ]
                        , Menu.item
                            [ onSelect ToStations
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    AddingStation fields ->
                        [ Menu.item
                            [ onSelect ToPlaying
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ onSelect ToChat
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "chat_bubble", text "Chat" ]
                        , Menu.item
                            [ onSelect ToStations
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    Chatting items ->
                        [ Menu.item
                            (case songQueue of
                                [] ->
                                    [ onSelect NoOp
                                    , Menu.disabled
                                    ]

                                _ ->
                                    [ onSelect ToPlaying
                                    , Options.cs "topMenuIcon"
                                    ]
                            )
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ onSelect ToStations
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ onSelect ToPreviousSongs
                            , Menu.disabled
                            , Menu.divider
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ onSelect Logout
                            , Options.cs "topMenuIcon"
                            ]
                            [ i "clear", text "Log out" ]
                        ]
                )
        ]


audioSelector : Float -> Bool -> List Song -> Html Msg
audioSelector modelAudioLevel modelAudioHover modelPreviousSongs =
    div
        [ onHover
        , onUnHover
        , style
            [ ( "display", "flex" )
            , ( "margin-right", "auto" )
            ]
        ]
        [ i
            [ style
                [ ( "margin-right", "auto" )
                , ( "margin-left", "10px" )
                , ( "margin-top", "5px" )
                , ( "z-index", "3" )
                , ( "-webkit-user-select", "none" )
                ]
            , class "material-icons topIcons"
            , (if modelAudioLevel == 0 then
                onClick UnMute
               else
                onClick Mute
              )
            ]
            [ text
                (if modelAudioLevel == 0 then
                    "volume_off"
                 else if modelAudioLevel > 0.5 then
                    "volume_up"
                 else
                    "volume_down"
                )
            ]
        , if modelAudioHover then
            audioSlider modelAudioLevel "visible"
          else
            audioSlider modelAudioLevel "hidden"
        ]


viewTopBar :
    Float
    -> String
    -> Bool
    -> Material.Model
    -> PlayingState
    -> List Song
    -> List Song
    -> Html Msg
viewTopBar modelAudioLevel modelCurrentStationName modelAudioHover modelMdl playingState modelPreviousSongs songQueue =
    div
        [ style
            [ ( "position", "relative" )
            , ( "height", "10%" )
            ]
        ]
        [ p
            [ style
                [ ( "text-align", "center" )
                , ( "position", "absolute" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "padding-top", "6px" )
                ]
            ]
            [ text modelCurrentStationName ]
        , div
            [ style
                [ ( "display", "flex" )
                ]
            ]
            [ audioSelector modelAudioLevel
                modelAudioHover
                modelPreviousSongs
            , viewTopMenu modelMdl playingState modelPreviousSongs songQueue
            ]
        ]


timePercentage : Float -> List Song -> Float
timePercentage currentTime songQueue =
    100
        * (currentTime
            / case (List.head songQueue) of
                Just song ->
                    toFloat song.trackLength

                Nothing ->
                    1.0
          )


viewProgressBar : Model -> Html Msg
viewProgressBar model =
    case model.state of
        LoggingIn fields ->
            div [] []

        Playing fields ->
            div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "10%" )
                    , ( "background-color", "black" )
                    , ( "cursor", "pointer" )
                    ]
                , onClickXVal
                , id "progressBar"
                ]
                [ div
                    [ style
                        [ ( "height", "100%" )
                        , ( "width"
                          , ((toString
                                (timePercentage
                                    fields.currentTime
                                    fields.songQueue
                                )
                             )
                                ++ "%"
                            )
                          )
                        , ( "background-color", "white" )
                        , ( "border-top", "1px black solid" )
                        ]
                    ]
                    []
                ]


ifEmptyQueue : Model -> Msg -> Msg
ifEmptyQueue model msg =
    case model.state of
        LoggingIn fields ->
            NoOp

        Playing fields ->
            (if fields.songQueue /= [] then
                msg
             else
                NoOp
            )


controller : Model -> Bool -> String -> List (Html Msg)
controller model notClickable stationId =
    case model.state of
        LoggingIn fields ->
            []

        Playing fields ->
            if notClickable then
                [ i
                    [ class "material-icons controlIcons"
                    , onClick (ifEmptyQueue model (SendThumbsDown stationId))
                    , style
                        [ ( "font-size", "20px" )
                        ]
                    ]
                    [ text "thumb_down_alt" ]
                , i
                    [ class "material-icons controlIcons"
                    , onClick (ifEmptyQueue model ReplaySong)
                    , style
                        [ ( "font-size", "26px" )
                        , ( "margin-left", "20px" )
                        ]
                    ]
                    [ text "replay" ]
                , i
                    [ class "material-icons controlIcons"
                    , onClick (ifEmptyQueue model TogglePause)
                    , style
                        [ ( "font-size", "40px" )
                        , ( "margin-left", "10px" )
                        , ( "margin-right", "10px" )
                        ]
                    ]
                    [ text
                        (if fields.isPlaying then
                            "pause"
                         else
                            "play_arrow"
                        )
                    ]
                , Icon.view "skip_next"
                    [ Options.cs "material-icons controlIcons"
                    , Options.css "font-size" "26px"
                    , Options.css "margin-right" "20px"
                    , Options.onClick (ifEmptyQueue model (SkipSong stationId))
                    , Tooltip.attach Mdl [ 690 ]
                    ]
                , Tooltip.render Mdl
                    [ 690 ]
                    model.mdl
                    [ Tooltip.top
                    , Options.css "width" "300px"
                    ]
                    [ div
                        [ style
                            [ ( "display", "flex" )
                            , ( "flex-direction", "column" )
                            ]
                        ]
                        [ img
                            [ style
                                [ ( "width", "100%" )
                                ]
                            , src
                                (case (List.head (List.drop 1 fields.songQueue)) of
                                    Just song ->
                                        song.albumArt

                                    Nothing ->
                                        ""
                                )
                            ]
                            []
                        , p [ style [ ( "text-align", "center" ), ( "margin-bottom", "0px" ) ] ]
                            [ text
                                (case (List.head (List.drop 1 fields.songQueue)) of
                                    Just song ->
                                        (song.songTitle ++ " - " ++ song.artistName)

                                    Nothing ->
                                        ""
                                )
                            ]
                        ]
                    ]
                , i
                    [ class "material-icons controlIcons"
                    , (if (currentSong model).rating == 1 then
                        onClick (RemoveThumbsUp stationId)
                       else
                        onClick (SendThumbsUp stationId)
                      )
                    , style
                        [ ( "font-size", "20px" )
                        , (if (currentSong model).rating == 1 then
                            ( "color", "#6499ef" )
                           else
                            ( "opacity", "1" )
                          )
                        ]
                    ]
                    [ text "thumb_up_alt" ]
                ]
            else
                [ i
                    [ class "material-icons controlIcons"
                    , style
                        [ ( "font-size", "20px" )
                        ]
                    ]
                    [ text "thumb_down_alt" ]
                , i
                    [ class "material-icons controlIcons"
                    , style
                        [ ( "font-size", "26px" )
                        , ( "margin-left", "20px" )
                        ]
                    ]
                    [ text "replay" ]
                , i
                    [ class "material-icons controlIcons"
                    , style
                        [ ( "font-size", "40px" )
                        , ( "margin-left", "10px" )
                        , ( "margin-right", "10px" )
                        ]
                    ]
                    [ text
                        "play_arrow"
                    ]
                , i
                    [ class "material-icons controlIcons"
                    , style
                        [ ( "font-size", "26px" )
                        , ( "margin-right", "20px" )
                        ]
                    ]
                    [ text "skip_next" ]
                , i
                    [ class "material-icons controlIcons"
                    , style
                        [ ( "font-size", "20px" )
                        , (if (currentSong model).rating == 1 then
                            ( "color", "#6499ef" )
                           else
                            ( "opacity", "1" )
                          )
                        ]
                    ]
                    [ text "thumb_up_alt" ]
                ]


viewControls : Model -> String -> Html Msg
viewControls model stationId =
    case model.state of
        LoggingIn fields ->
            div [] []

        Playing fields ->
            div
                [ style
                    [ ( "margin-top", "auto" )
                    , ( "-webkit-user-select", "none" )
                    , ( "height", "10%" )
                    ]
                ]
                [ viewProgressBar model
                , (case fields.playingState of
                    Normal ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                ]
                            ]
                            (controller model True stationId)

                    SelectingPlaying ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                , ( "cursor", "pointer" )
                                ]
                            , onClick ToPlaying
                            ]
                            (controller model True stationId)

                    SelectingStation ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                ]
                            ]
                            (controller model False stationId)

                    PreviousSongs ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                , ( "cursor", "pointer" )
                                ]
                            , onClick ToPlaying
                            ]
                            (controller model True stationId)

                    AddingStation fields ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                , ( "cursor", "pointer" )
                                ]
                            ]
                            (controller model True stationId)

                    Chatting items ->
                        div
                            [ style
                                [ ( "width", "100%" )
                                , ( "height", "90%" )
                                , ( "background-color", "#323842" )
                                , ( "display", "flex" )
                                , ( "justify-content", "center" )
                                , ( "align-items", "center" )
                                , ( "cursor", "pointer" )
                                ]
                            ]
                            (controller model True stationId)
                  )
                ]


viewSongInfo : Model -> Html Msg
viewSongInfo model =
    case model.state of
        LoggingIn fields ->
            div [] []

        Playing fields ->
            if model.startStationError then
                h3 [ style [ ( "text-align", "center" ) ] ] [ text "Error starting station :(" ]
            else if model.loadingSongError then
                h3 [ style [ ( "text-align", "center" ) ] ] [ text "Error loading song :(" ]
            else
                div
                    [ style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "align-items", "center" )
                        , ( "margin-top", "1%" )
                        , ( "height", "60%" )
                        ]
                    ]
                    [ img
                        [ style
                            [ ( "-webkit-user-select", "none" )
                            , ( "height", "100%" )
                            , ( "box-shadow", "0.25px 0.25px black" )
                            , ( "border", "1px black solid" )
                            ]
                        , src
                            (case (List.head fields.songQueue) of
                                Just song ->
                                    song.albumArt

                                Nothing ->
                                    "fillerIMG.jpg"
                            )
                        , class "playerAlbumArt"
                        ]
                        []
                    , p
                        [ style
                            [ ( "margin-top", "4%" )
                            , ( "margin-bottom", "0px" )
                            ]
                        , class "songTitle"
                        ]
                        [ (case (List.head fields.songQueue) of
                            Just song ->
                                text song.songTitle

                            Nothing ->
                                if fields.currentStation == Nothing then
                                    text "Select a station to listen"
                                else
                                    Spinner.spinner [ Spinner.active True ]
                          )
                        ]
                    , p
                        [ style
                            [ ( "margin-top", "5px" )
                            ]
                        , class "songDetails"
                        ]
                        [ text
                            (case (List.head fields.songQueue) of
                                Just song ->
                                    song.artistName ++ " - " ++ song.albumTitle

                                Nothing ->
                                    ""
                            )
                        ]
                    ]


viewLogin : Model -> Bool -> Bool -> Html Msg
viewLogin model remember failed =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "width", "650px" )
            , ( "height", "100%" )
            , ( "margin", "auto" )
            , ( "margin-top", "10px" )
            , ( "padding-top", "30px" )
            , ( "padding-bottom", "30px" )
            ]
        ]
        [ h3
            [ style
                [ ( "margin-top", "0" )
                , ( "text-align", "center" )
                ]
            ]
            [ text "Log in" ]
        , input
            [ style
                [ ( "margin-bottom", "30px" )
                , ( "margin-top", "25px" )
                , ( "width", "450px" )
                , ( "align-self", "center" )
                , ( "height", "35px" )
                ]
            , type_ "email"
            , placeholder "Email"
            , onInput InputEmail
            ]
            []
        , input
            [ type_ "password"
            , placeholder "Password"
            , onInput InputPassword
            , style
                [ ( "width", "450px" )
                , ( "align-self", "center" )
                , ( "height", "35px" )
                , ( "margin-bottom", "30px" )
                ]
            ]
            []
        , div
            [ style
                [ ( "margin-top", "10%" )
                , ( "height", "40px" )
                , ( "width", "460px" )
                , ( "align-self", "center" )
                , ( "display", "flex" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "margin-left", "10px" )
                    , ( "margin-right", "auto" )
                    , ( "margin-top", "10px" )
                    ]
                ]
                [ p [] [ text "Remember me" ]
                , input
                    [ type_ "checkbox"
                    , onCheck RememberMe
                    , style
                        [ ( "margin-top", "7px" )
                        , ( "margin-left", "5px" )
                        ]
                    ]
                    []
                ]
            , Button.render Mdl
                [ 102321 ]
                model.mdl
                [ if remember then
                    Options.onClick LoginRemember
                  else
                    Options.onClick Login
                , Button.raised
                , Button.ripple
                , Options.css "margin-right" "10px"
                , Options.css "margin-left" "auto"
                ]
                [ text "Log in" ]
            ]
        , if failed then
            p
                [ style
                    [ ( "color", "red" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ text "Invalid username or password" ]
          else
            text ""
        ]


viewStation : String -> String -> String -> Grid.Cell Msg
viewStation id name url =
    Grid.cell
        [ Grid.size Desktop 3
        , Options.css "margin-bottom" "25px "
        , Options.css "text-align" "center"
        ]
        [ div
            [ style
                [ ( "width", "150px" )
                , ( "height", "175px" )
                , ( "margin", "auto" )
                ]
            , onClick (StartStation id name url)
            ]
            [ img
                [ style
                    [ ( "height", "150px" )
                    , ( "width", "150px" )
                    , ( "border", "1px solid black" )
                    , ( "box-shadow", "0.25px 0.25px black" )
                    , ( "-webkit-user-select", "none" )
                    ]
                , src url
                ]
                []
            , p
                [ style
                    [ ( "text-align", "center" )
                    ]
                ]
                [ text name ]
            ]
        ]


viewPreviousSong : Song -> Grid.Cell Msg
viewPreviousSong song =
    Grid.cell
        [ Grid.size Desktop 3
        , Options.css "margin-bottom" "25px "
        , Options.css "text-align" "center"
        ]
        [ div
            [ style
                [ ( "width", "150px" )
                , ( "height", "175px" )
                , ( "margin", "auto" )
                ]
            , onClick (PlayPreviousSong song)
            ]
            [ img
                [ style
                    [ ( "height", "150px" )
                    , ( "width", "150px" )
                    , ( "border", "1px solid black" )
                    , ( "box-shadow", "0.25px 0.25px black" )
                    , ( "-webkit-user-select", "none" )
                    ]
                , src song.albumArt
                ]
                []
            , p
                [ style
                    [ ( "text-align", "center" )
                    ]
                ]
                [ text song.songTitle ]
            ]
        ]


viewPreviousSongs : Model -> Html Msg
viewPreviousSongs model =
    case model.state of
        LoggingIn record ->
            div [] []

        Playing record ->
            Grid.grid
                [ Options.css "width" "100%"
                , Options.css "margin" "0px"
                , Options.css "overflow-y" "auto"
                ]
                (List.map
                    (\song ->
                        viewPreviousSong
                            song
                    )
                    record.previousSongs
                )


viewAddStation : Grid.Cell Msg
viewAddStation =
    Grid.cell
        [ Grid.size Desktop 3
        , Options.css "margin-bottom" "25px "
        , Options.css "text-align" "center"
        , Options.onClick ToSongSearch
        ]
        [ div
            [ style
                [ ( "width", "150px" )
                , ( "height", "175px" )
                , ( "margin", "auto" )
                ]
            ]
            [ img
                [ style
                    [ ( "height", "150px" )
                    , ( "width", "150px" )
                    , ( "border", "1px solid black" )
                    , ( "box-shadow", "0.25px 0.25px black" )
                    , ( "-webkit-user-select", "none" )
                    ]
                , src "add-station.png"
                ]
                []
            , p
                [ style
                    [ ( "text-align", "center" )
                    ]
                ]
                [ text "Add a station" ]
            ]
        ]


viewStationSelector : Model -> List Station -> Html Msg
viewStationSelector model stations =
    if model.getStationError then
        h3 [ style [ ( "text-align", "center" ) ] ] [ text "Error retrieving stations :(" ]
    else
        Grid.grid
            [ Options.css "width" "100%"
            , Options.css "margin" "0px"
            , Options.css "overflow-y" "auto"
            ]
            (viewAddStation
                :: (List.map
                        (\station ->
                            viewStation
                                station.id
                                station.name
                                station.art
                        )
                        stations
                   )
            )


viewPlayer :
    Model
    -> Html Msg
    -> Float
    -> Maybe Station
    -> Bool
    -> Material.Model
    -> PlayingState
    -> List Song
    -> List Song
    -> Html Msg
viewPlayer model content audioLevel currentStation audioHover mdl playingState previousSongs songQueue =
    div
        [ style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "margin", "auto" )
            ]
        ]
        [ viewTopBar audioLevel
            (case currentStation of
                Just station ->
                    station.name

                Nothing ->
                    ""
            )
            audioHover
            mdl
            playingState
            previousSongs
            songQueue
        , content
        , viewControls model (getCurrentStation model).id
        ]


darkEle : Html Msg
darkEle =
    div
        [ style
            [ ( "z-index", "2" )
            , ( "position", "absolute" )
            , ( "background-color", "rgba(0, 0, 0, 0.5)" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            , ( "right", "0" )
            , ( "left", "0" )
            ]
        ]
        []


chatInput : Model -> Html Msg
chatInput model =
    case model.state of
        LoggingIn fields ->
            text ""

        Playing fields ->
            case fields.playingState of
                Normal ->
                    text ""

                SelectingPlaying ->
                    text ""

                SelectingStation ->
                    text ""

                AddingStation fields ->
                    text ""

                PreviousSongs ->
                    text ""

                Chatting fields ->
                    div
                        [ style
                            [ ( "height", "6%" )
                            , ( "width", "89.75%" )
                            , ( "margin-top", "1%" )
                            , ( "margin-bottom", "3%" )
                            , ( "display", "flex" )
                            ]
                        ]
                        [ input
                            [ style
                                [ ( "padding-left", "5px" )
                                , ( "height", "100%" )
                                , ( "width", "95%" )
                                ]
                            , value
                                fields.chatInput
                            , onInput ChatInput
                            , Html.Events.onWithOptions "keydown"
                                { stopPropagation = True
                                , preventDefault = False
                                }
                                (Decode.succeed NoOp)
                            ]
                            []
                        , (Button.render Mdl
                            [ 7834 ]
                            model.mdl
                            [ Button.ripple
                            , Options.css "height" "100%"
                            , Options.css "margin-top" ".45%"
                            , Options.onClick SendChat
                            ]
                            [ text "Send" ]
                          )
                        ]


viewChatRooms : Html Msg
viewChatRooms =
    div
        [ style
            [ ( "background-color", "rgb(223, 223, 223)" )
            , ( "width", "15%" )
            , ( "height", "95%" )
            ]
        ]
        []


viewChat : Chat -> Html Msg
viewChat chat =
    div
        [ style
            [ ( "display", "flex" )
            , ( "margin-left", "1%" )
            ]
        ]
        [ if chat.username /= "" then
            p
                [ style
                    [ ( "font-weight", "bold" )
                    ]
                ]
                [ text chat.username ]
          else
            p
                [ style
                    [ ( "font-weight", "bold" )
                    ]
                ]
                [ text chat.email ]
        , p
            [ style
                [ ( "margin-left", "1.5%" )
                , ( "margin-top", "0.04%" )
                ]
            ]
            [ text chat.content ]
        ]


chatLogin : Model -> String -> Html Msg
chatLogin model nameInput =
    div
        [ style
            [ ( "z-index", "2" )
            , ( "position", "absolute" )
            , ( "background-color", "white" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "height", "60%" )
            , ( "width", "50%" )
            ]
        ]
        [ h3 [ style [ ( "text-align", "center" ) ] ]
            [ text "Please enter a username: " ]
        , p [ style [ ( "text-align", "center" ) ] ]
            [ text "This is what other users will see when you send a message" ]
        , p
            [ style
                [ ( "text-align", "center" )
                , ( "margin-top", "10%" )
                ]
            , class "chatLoginAltOpt"
            ]
            [ text "Or click here to use your email instead" ]
        , input
            [ type_ "text"
            , onInput UserNameInput
            , style
                [ ( "height", "12%" )
                , ( "padding-left", "2%" )
                , ( "font-size", "100%" )
                , ( "margin-top", "auto" )
                ]
            ]
            []
        , (Button.render Mdl
            [ 18 ]
            model.mdl
            [ Button.minifab
            , Button.ripple
            , (if nameInput == "" then
                Options.onClick NoOp
               else
                Options.onClick SetUserName
              )
            , Options.css "height" "20%"
            , Options.css "margin-bottom" "0px"
            ]
            [ Icon.i "arrow_right_alt" ]
          )
        ]


viewAllChats : List Chat -> List (Html Msg)
viewAllChats chatList =
    List.map viewChat chatList


viewChatWindow : Model -> Html Msg
viewChatWindow model =
    case model.state of
        LoggingIn record ->
            text ""

        Playing record ->
            case record.playingState of
                Normal ->
                    text ""

                SelectingStation ->
                    text ""

                SelectingPlaying ->
                    text ""

                AddingStation fields ->
                    text ""

                PreviousSongs ->
                    text ""

                Chatting fields ->
                    div
                        [ style
                            [ ( "height", "100%" )
                            , ( "width", "100%" )
                            , ( "display", "flex" )
                            , ( "flex-direction", "column" )
                            , ( "align-items", "center" )
                            , ( "justify-content", "center" )
                            ]
                        ]
                        [ div
                            [ style
                                [ ( "display", "flex" )
                                , ( "height", "100%" )
                                , ( "width", "100%" )
                                ]
                            ]
                            [ viewChatRooms

                            -- CHAT SELECTOR ^^^
                            , div
                                [ style
                                    [ ( "margin-top", "0px" )
                                    , ( "width", "84%" )
                                    , ( "height", "95%" )
                                    , ( "overflow-y", "auto" )
                                    ]
                                ]
                                (viewAllChats
                                    fields.chats
                                )
                            ]

                        -- CHAT ^^^
                        , chatInput model
                        , if fields.newUser then
                            darkEle
                          else
                            text ""
                        , if fields.newUser then
                            chatLogin model fields.userNameInput
                          else
                            text ""
                        ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    case model.state of
        LoggingIn record ->
            text ""

        Playing record ->
            case record.playingState of
                Normal ->
                    text ""

                SelectingPlaying ->
                    text ""

                SelectingStation ->
                    text ""

                PreviousSongs ->
                    text ""

                AddingStation fields ->
                    div
                        [ style
                            [ ( "overflow-y", "auto" )
                            , ( "width", "100%" )
                            ]
                        ]
                        (List.map viewSearchResult (Dict.values fields.searchedSongs))

                Chatting items ->
                    text ""


viewSearchResult : SearchResult -> Html Msg
viewSearchResult result =
    div
        [ style
            [ ( "display", "flex" )
            , ( "width", "100%" )
            , ( "border-top", "0.5px black solid" )
            ]
        , onClick (CreateStation result.pandoraId)
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "justify-content", "space-around" )
                , ( "margin-left", "3%" )
                ]
            ]
            [ (case result.artistName of
                Just artist ->
                    p [ style [ ( "margin", "0" ) ] ]
                        [ text artist
                        ]

                Nothing ->
                    text ""
              )
            , p [ style [ ( "margin", "0" ) ] ] [ text result.name ]
            , p [ style [ ( "margin", "0" ) ] ]
                [ text
                    (case result.resultType of
                        "AR" ->
                            "Artist"

                        "TR" ->
                            "Song"

                        "AL" ->
                            "Album"

                        _ ->
                            ""
                    )
                ]
            ]
        , img
            [ src
                (case
                    (case result.resultType of
                        "AR" ->
                            result.art

                        _ ->
                            result.thorId
                    )
                 of
                    Just art ->
                        ("https://content-images.p-cdn.com/"
                            ++ art
                        )

                    Nothing ->
                        "https://vignette.wikia.nocookie.net/the-darkest-minds/images/4/47/Placeholder.png/revision/latest?cb=20160927044640"
                )
            , style
                [ ( "margin-left", "auto" )
                , ( "border-left", "0.5px black solid" )
                , ( "border-right", "0.5px black solid" )
                , ( "margin-right", "3%" )
                , ( "height", "150px" )
                , ( "width", "150px" )
                ]
            ]
            []
        ]


viewSongSearch : Model -> Html Msg
viewSongSearch model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "height", "100%" )
            , ( "width", "100%" )
            , ( "align-items", "center" )
            ]
        ]
        [ input
            [ style
                [ ( "height", "7.5%" )
                , ( "width", "70%" )
                , ( "padding-left", "1.5%" )
                ]
            , onInput SearchForSong
            ]
            []
        , viewSearchResults model
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Playing fields ->
            div
                [ style [ ( "height", "100%" ) ] ]
                [ audio
                    [ onEnded
                        (case (fields.currentStation) of
                            Just station ->
                                station.id

                            Nothing ->
                                ""
                        )
                    , id "songAudio"
                    , src
                        (case
                            (List.head
                                (case model.state of
                                    Playing fields ->
                                        fields.songQueue

                                    LoggingIn fields ->
                                        []
                                )
                            )
                         of
                            Just song ->
                                song.audioURL

                            Nothing ->
                                ""
                        )
                    , autoplay True
                    ]
                    []
                , viewPlayer model
                    (case fields.playingState of
                        Normal ->
                            viewSongInfo model

                        SelectingPlaying ->
                            viewStationSelector model fields.stations

                        SelectingStation ->
                            viewStationSelector model fields.stations

                        PreviousSongs ->
                            viewPreviousSongs model

                        AddingStation fields ->
                            viewSongSearch model

                        Chatting items ->
                            viewChatWindow model
                    )
                    fields.audioLevel
                    fields.currentStation
                    fields.audioHover
                    model.mdl
                    fields.playingState
                    fields.previousSongs
                    fields.songQueue
                ]

        LoggingIn fields ->
            viewLogin model fields.remember fields.failed
