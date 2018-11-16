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
import Types.Station as Station exposing (Station)
import Types.Fragment as Fragment exposing (Song)


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

        Failed error ->
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
    | PreviousSongs


type State
    = LoggingIn
        { email : String
        , password : String
        , remember : Bool
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
        }
    | Failed String


stateCase : Model -> a -> a -> a -> a
stateCase model playing loggingIn failed =
    case model.state of
        Playing fields ->
            playing

        LoggingIn fields ->
            loggingIn

        Failed error ->
            failed


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

        Failed fields ->
            { id = ""
            , name = "Select a station"
            , art = ""
            }


type alias Model =
    { state : State
    , mdl : Material.Model
    }


type alias UserInfo =
    { authToken : String
    , email : String
    }


type alias User =
    { email : String
    , authToken : String
    , audioLevel : Maybe Float
    , lastStation : Maybe String
    }


type alias Flags =
    { authToken : Maybe String
    , audioLevel : Maybe Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case flags.authToken of
        Just token ->
            { state =
                Playing
                    { authToken = token
                    , seek = 0
                    , stations = []
                    , currentStation = Nothing
                    , songQueue = []
                    , currentTime = 0.0
                    , previousSongs = []
                    , audioLevel =
                        case flags.audioLevel of
                            Just level ->
                                level

                            Nothing ->
                                1.0
                    , audioHover = False
                    , isPlaying = False
                    , playingState = SelectingStation
                    }
            , mdl = Material.model
            }
                ! [ Http.send GotStations (Station.get token) ]

        Nothing ->
            { state =
                LoggingIn
                    { email = ""
                    , password = ""
                    , remember = False
                    }
            , mdl = Material.model
            }
                ! []



-- ΩΩΩ UPDATE ΩΩΩ


authDecode : Decode.Decoder String
authDecode =
    (Decode.at [ "authToken" ] Decode.string)


login : { email : String, password : String } -> Request String
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
    | BackToStations
    | BackToPlaying
    | ToPreviousSongs
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

                Failed error ->
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

                Failed error ->
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

                Failed error ->
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
                          ]

                Playing fields ->
                    model ! []

                Failed error ->
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
                                        , audioLevel = 1.0
                                        , audioHover = False
                                        , isPlaying = False
                                        , playingState = SelectingStation
                                        , seek = 0
                                        }
                            }
                                ! [ Http.send GotStations
                                        (Station.get token)
                                  , rememberMe token
                                  ]

                        Err error ->
                            { model | state = Failed ("Error logging in: " ++ toString error) } ! []

                Playing fields ->
                    model ! []

                Failed error ->
                    model ! []

        LoggedIn result ->
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
                                        , audioLevel = 1.0
                                        , audioHover = False
                                        , isPlaying = False
                                        , playingState = SelectingStation
                                        , seek = 0
                                        }
                            }
                                ! [ Http.send
                                        GotStations
                                        (Station.get token)
                                  ]

                        Err error ->
                            { model | state = Failed ("Error logging in: " ++ toString error) } ! []

                Playing fields ->
                    model ! []

                Failed error ->
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
                            { model | state = Failed ("Error loading stations: " ++ toString error) } ! []

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

        LoadNextSongs id ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    model
                        ! [ Http.send LoadedNextSongs
                                (Fragment.getNext id fields.authToken False)
                          ]

                Failed error ->
                    model ! []

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
                            { model | state = Failed ("Error starting station: " ++ toString error) }
                                ! []

                Failed error ->
                    model ! []

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
                            { model | state = Failed ("Error loading songs: " ++ toString error) } ! []

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

        BackToStations ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | playingState = SelectingPlaying }
                    }
                        ! []

                Failed error ->
                    model ! []

        Mdl msg ->
            Material.update Mdl msg model

        NoOp ->
            model ! []

        BackToPlaying ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | playingState = Normal }
                    }
                        ! []

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

        Logout ->
            { model
                | state =
                    LoggingIn
                        { email = ""
                        , password = ""
                        , remember = False
                        }
            }
                ! [ logOutLocalStorage () ]

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

                Failed error ->
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

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []

        ToPreviousSongs ->
            case model.state of
                LoggingIn record ->
                    model ! []

                Playing record ->
                    { model | state = Playing { record | playingState = PreviousSongs } } ! []

                Failed error ->
                    model ! []

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

                Failed error ->
                    model ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


port togglePause : () -> Cmd msg


port replaySong : () -> Cmd msg


port audioLevel : Float -> Cmd msg


port rememberMe : String -> Cmd msg


port getProgressBarWidth : () -> Cmd msg


port sendProgressBarWidth : (Float -> msg) -> Sub msg


port sendNewTime : Float -> Cmd msg


port logOutLocalStorage : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        LoggingIn fields ->
            Sub.batch [ Sub.none ]

        Playing fields ->
            Sub.batch
                [ if fields.isPlaying then
                    Time.every Time.second SetCurrentTime
                  else
                    Sub.none
                , Material.subscriptions Mdl model
                , sendProgressBarWidth SetNewTime
                ]

        Failed error ->
            Sub.batch [ Sub.none ]



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


viewTopMenu : Material.Model -> PlayingState -> List Song -> Html Msg
viewTopMenu modelMdl playingState previousSongs =
    div
        [ style
            [ ( "margin-left", "auto" )
            , ( "-webkit-user-select", "none" )
            ]
        ]
        [ let
            i name =
                Icon.view name [ Options.css "width" "30px" ]

            padding =
                Options.css "padding-right" "20px"
          in
            Menu.render Mdl
                [ 2 ]
                modelMdl
                [ Menu.bottomRight, Menu.icon "keyboard_arrow_down" ]
                (case playingState of
                    Normal ->
                        [ Menu.item
                            [ Menu.onSelect BackToStations
                            , padding
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ Menu.onSelect ToPreviousSongs
                            , (if previousSongs == [] then
                                Menu.disabled
                               else
                                Options.disabled False
                              )
                            , padding
                            , Menu.divider
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ Menu.onSelect Logout
                            , padding
                            ]
                            [ i "clear", text "Log out" ]
                        ]

                    SelectingPlaying ->
                        [ Menu.item
                            [ Menu.onSelect BackToPlaying
                            , padding
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ Menu.onSelect ToPreviousSongs
                            , padding
                            , Menu.divider
                            , (if previousSongs == [] then
                                Menu.disabled
                               else
                                Options.disabled False
                              )
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ Menu.onSelect Logout, padding ]
                            [ i "clear", text "Log out" ]
                        ]

                    SelectingStation ->
                        [ Menu.item
                            [ Menu.onSelect BackToPlaying
                            , padding
                            , Menu.disabled
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ Menu.onSelect ToPreviousSongs
                            , Menu.disabled
                            , padding
                            , Menu.divider
                            ]
                            [ i "first_page", text "Previous Songs" ]
                        , Menu.item
                            [ Menu.onSelect Logout, padding ]
                            [ i "clear", text "Log out" ]
                        ]

                    PreviousSongs ->
                        [ Menu.item
                            [ Menu.onSelect BackToPlaying
                            , padding
                            ]
                            [ i "play_circle_outline", text "Player" ]
                        , Menu.item
                            [ Menu.onSelect BackToStations
                            , padding
                            ]
                            [ i "apps", text "Stations" ]
                        , Menu.item
                            [ Menu.onSelect Logout, padding ]
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
    -> Html Msg
viewTopBar modelAudioLevel modelCurrentStationName modelAudioHover modelMdl playingState modelPreviousSongs =
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
            , viewTopMenu modelMdl playingState modelPreviousSongs
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

        Failed error ->
            text error


controller : Model -> Bool -> String -> List (Html Msg)
controller model notClickable stationId =
    case model.state of
        LoggingIn fields ->
            []

        Playing fields ->
            if notClickable then
                [ i
                    [ class "material-icons controlIcons"
                    , onClick (SendThumbsDown stationId)
                    , style
                        [ ( "font-size", "20px" )
                        ]
                    ]
                    [ text "thumb_down_alt" ]
                , i
                    [ class "material-icons controlIcons"
                    , onClick ReplaySong
                    , style
                        [ ( "font-size", "26px" )
                        , ( "margin-left", "20px" )
                        ]
                    ]
                    [ text "replay" ]
                , i
                    [ class "material-icons controlIcons"
                    , onClick TogglePause
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
                    , Options.onClick (SkipSong stationId)
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
                    , onClick (SkipSong stationId)
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

        Failed error ->
            [ text error ]


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
                            , onClick BackToPlaying
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
                            , onClick BackToPlaying
                            ]
                            (controller model True stationId)
                  )
                ]

        Failed error ->
            text error


viewSongInfo : Model -> Html Msg
viewSongInfo model =
    case model.state of
        LoggingIn fields ->
            div [] []

        Playing fields ->
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
                        ]
                    , src
                        (case (List.head fields.songQueue) of
                            Just song ->
                                song.albumArt

                            Nothing ->
                                "fillerIMG.jpg"
                        )
                    , style
                        [ ( "border", "1px black solid" )
                        ]
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

        Failed error ->
            text error


viewLogin : Model -> Bool -> Html Msg
viewLogin model remember =
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

        Failed error ->
            text error


viewStationSelector : Model -> List Station -> Html Msg
viewStationSelector model stations =
    Grid.grid
        [ Options.css "width" "100%"
        , Options.css "margin" "0px"
        , Options.css "overflow-y" "auto"
        ]
        (List.map
            (\station ->
                viewStation
                    station.id
                    station.name
                    station.art
            )
            stations
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
    -> Html Msg
viewPlayer model content audioLevel currentStation audioHover mdl playingState previousSongs =
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
        , content
        , viewControls model (getCurrentStation model).id
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

                                    Failed error ->
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
                    )
                    fields.audioLevel
                    fields.currentStation
                    fields.audioHover
                    model.mdl
                    fields.playingState
                    fields.previousSongs
                ]

        LoggingIn fields ->
            viewLogin model fields.remember

        Failed error ->
            div []
                [ p
                    [ onClick Logout
                    , style
                        [ ( "cursor", "pointer" )
                        ]
                    , class "logOut"
                    ]
                    [ text "Logout" ]
                , p
                    [ style
                        [ ( "text-align", "center" )
                        ]
                    ]
                    [ text error ]
                ]
