module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Http exposing (Request)
import Material
import Material.Slider as Slider
import Material.Spinner as Spinner
import Material.Options as Options
import Material.Button as Button
import Material.Menu as Menu
import Material.Grid as Grid exposing (Device(..))
import Material.Tooltip as Tooltip
import Material.Icon as Icon
import Time
import Util.Types as Types exposing (Song, Station, GlobalModel, SearchResult)
import Keyboard
import Dict exposing (Dict)
import Util.Events as Events


main : Program Flags GlobalModel Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- ΩΩΩ MODEL ΩΩΩ


currentSong : GlobalModel -> Song
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


type alias AddingStationFields =
    { searchResults : Dict String SearchResult
    , searchInput : String
    }


type alias PlayingFields =
    { authToken : String
    , currentStation : Maybe Station
    , songQueue : List Song
    , currentTime : Float
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


getCurrentStation : GlobalModel -> Station
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


type State
    = LoggingIn LoggingInFields
    | Playing PlayingFields


type PlayingState
    = Normal
    | SelectingStation
    | SelectingPlaying
    | AddingStation AddingStationFields
    | PreviousSongs
    | Chatting ChattingFields


type alias Flags =
    { password : Maybe String
    , audioLevel : Maybe Float
    , username : Maybe String
    , email : Maybe String
    , newUser : Maybe String
    }


init : Flags -> ( GlobalModel, Cmd Msg )
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
    | ToPlaying
    | ToChat
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
      -- Removing Stations
    | OpenRemoveStationPopup
    | CloseRemoveStationPopup
    | RemoveStation Station
    | RemovedStation (Result Http.Error String)
      -- Updating Stations
    | UpdateStation
    | UpdatedStation (Result Http.Error String)
    | OpenUpdateStationPopup
    | CloseUpdateStationPopup
    | NameInputToModel String


update : Msg -> GlobalModel -> ( GlobalModel, Cmd Msg )
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
                                        , deletingStationPopup = False
                                        , updatingStationPopup = False
                                        , updateStationNameInput = ""
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
                                        , deletingStationPopup = False
                                        , updatingStationPopup = False
                                        , updateStationNameInput = ""
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

        LoadNextSongs id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    model
                        ! [ Http.send LoadedNextSongs
                                (Fragment.getNext id fields.authToken False)
                          ]

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
                            ! [ rememberUsername record.username
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

        NoOp1 _ ->
            model ! []

        RemoveStation removedStation ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model
                        | state =
                            Playing
                                { record
                                    | stations =
                                        (List.filter
                                            (\station -> station.id /= removedStation.id)
                                            record.stations
                                        )
                                }
                    }
                        ! [ Http.send RemovedStation (Station.remove removedStation.id record.authToken) ]

        RemovedStation _ ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model
                        | state =
                            Playing
                                { record
                                    | playingState = SelectingStation
                                    , deletingStationPopup = False
                                    , isPlaying = False
                                    , currentTime = 0
                                    , currentStation = Nothing
                                }
                    }
                        ! []

        OpenRemoveStationPopup ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | deletingStationPopup = True } } ! []

        CloseRemoveStationPopup ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | deletingStationPopup = False } } ! []

        OpenUpdateStationPopup ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | updatingStationPopup = True } } ! []

        CloseUpdateStationPopup ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | updatingStationPopup = False } } ! []

        UpdateStation ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    let
                        stationId =
                            case record.currentStation of
                                Just station ->
                                    station.id

                                Nothing ->
                                    ""
                    in
                        model ! [ Http.send UpdatedStation (Station.update stationId record.updateStationNameInput record.authToken) ]

        UpdatedStation result ->
            case result of
                Ok result ->
                    case model.state of
                        LoggingIn record ->
                            model ! []

                        Playing record ->
                            let
                                station =
                                    case record.currentStation of
                                        Just station ->
                                            station

                                        Nothing ->
                                            { id = ""
                                            , name = ""
                                            , art = ""
                                            }
                            in
                                { model
                                    | state =
                                        Playing
                                            { record
                                                | currentStation =
                                                    Just
                                                        { id = station.id
                                                        , name = record.updateStationNameInput
                                                        , art = station.art
                                                        }
                                                , updatingStationPopup = False
                                            }
                                }
                                    ! []

                Err error ->
                    let
                        log =
                            Debug.log "Error updating station" error
                    in
                        model ! []

        NameInputToModel input ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | updateStationNameInput = input } } ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : GlobalModel -> Sub Msg
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
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "padding-top", "6px" )
                ]
            ]
            [ i
                [ class "material-icons deleteStation"
                , style
                    [ ( "margin-right", "1%" )
                    , (case modelCurrentStationName of
                        "" ->
                            ( "opacity", "0" )

                        _ ->
                            ( "pointer-events", "auto" )
                      )
                    , (case modelCurrentStationName of
                        "" ->
                            ( "pointer-events", "none" )

                        _ ->
                            ( "pointer-events", "auto" )
                      )
                    ]
                , onClick OpenRemoveStationPopup
                ]
                [ text "delete" ]
            , p [] [ text modelCurrentStationName ]
            , i
                [ class "material-icons editStation"
                , onClick OpenUpdateStationPopup
                , style
                    [ ( "margin-left", "1%" )
                    , (case modelCurrentStationName of
                        "" ->
                            ( "opacity", "0" )

                        _ ->
                            ( "pointer-events", "auto" )
                      )
                    , (case modelCurrentStationName of
                        "" ->
                            ( "pointer-events", "none" )

                        _ ->
                            ( "pointer-events", "auto" )
                      )
                    ]
                ]
                [ text "edit" ]
            ]
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


viewProgressBar : GlobalModel -> Html Msg
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


ifEmptyQueue : GlobalModel -> Msg -> Msg
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


controller : GlobalModel -> Bool -> String -> List (Html Msg)
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


viewControls : GlobalModel -> String -> Html Msg
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


viewSongInfo : GlobalModel -> Html Msg
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


viewLogin : GlobalModel -> Bool -> Bool -> Html Msg
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


viewPlayer :
    GlobalModel
    -> Html Msg
    -> Float
    -> Maybe Station
    -> Bool
    -> Material.Model
    -> PlayingState
    -> List Song
    -> List Song
    -> Bool
    -> Bool
    -> Html Msg
viewPlayer model content audioLevel currentStation audioHover mdl playingState previousSongs songQueue removingStationPopup updatingStationPopup =
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
        , (if removingStationPopup then
            Station.removingPopup mdl
                Mdl
                (case currentStation of
                    Just station ->
                        station

                    Nothing ->
                        { id = ""
                        , name = ""
                        , art = ""
                        }
                )
                CloseRemoveStationPopup
                RemoveStation
           else
            text ""
          )
        , (if updatingStationPopup then
            Station.updatingPopup mdl
                Mdl
                (case currentStation of
                    Just station ->
                        station

                    Nothing ->
                        { id = ""
                        , name = ""
                        , art = ""
                        }
                )
                CloseUpdateStationPopup
                UpdateStation
                NoOp
                NameInputToModel
           else
            text ""
          )
        ]


view : GlobalModel -> Html Msg
view model =
    case model.state of
        Playing record ->
            div
                [ style [ ( "height", "100%" ) ] ]
                [ audio
                    [ onEnded
                        (case (record.currentStation) of
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
                    (case record.playingState of
                        Normal ->
                            viewSongInfo model

                        SelectingPlaying ->
                            viewStationSelector model record.stations

                        SelectingStation ->
                            viewStationSelector model record.stations

                        PreviousSongs ->
                            viewPreviousSongs model

                        AddingStation fields ->
                            Station.viewSearch fields.searchResults SearchForSong NoOp CreateStation

                        Chatting items ->
                            Chat.viewWindow items
                                ChatInput
                                Mdl
                                model.mdl
                                SendChat
                                UserNameInput
                                NoOp
                                SetUserName
                    )
                    record.audioLevel
                    record.currentStation
                    record.audioHover
                    model.mdl
                    record.playingState
                    record.previousSongs
                    record.songQueue
                    record.deletingStationPopup
                    record.updatingStationPopup
                ]

        LoggingIn record ->
            viewLogin model record.remember record.failed
