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
import Material
import Time
import Types.Feedback as Feedback
import Types.Station as Station exposing (Station)
import Types.Fragment as Fragment exposing (Song)


main : Program String Model Msg
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


stateCase : Model -> a -> a -> a
stateCase model playing loggingIn =
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


init : String -> ( Model, Cmd Msg )
init userToken =
    if userToken == "" then
        { state =
            LoggingIn
                { email = ""
                , password = ""
                , remember = False
                }
        , mdl = Material.model
        }
            ! []
    else
        { state =
            Playing
                { authToken = userToken
                , seek = 0
                , stations = []
                , currentStation = Nothing
                , songQueue = []
                , currentTime = 0.0
                , previousSongs = []
                , audioLevel = 1
                , audioHover = False
                , isPlaying = False
                , playingState = SelectingStation
                }
        , mdl = Material.model
        }
            ! [ Http.send GotStations (Station.get userToken) ]



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


type Msg
    = InputEmail String
    | InputPassword String
    | Login
    | LoginRemember
    | LoggedIn (Result Http.Error String)
    | GotStations (Result Http.Error (List Station))
    | StartStation String String String
    | LoadNextSongs String
    | LoadedNextSongs (Result Http.Error (List Song))
    | StartedStation (Result Http.Error (List Song))
    | SongEnded String
    | SkipSong String
    | SetCurrentTime Float
    | ReplaySong
    | SendThumbsDown String
    | SendThumbsUp String
    | SentFeedBack (Result Http.Error String)
    | SetAudio Float
    | HoveringAudio
    | UnHoveringAudio
    | RemoveThumbsUp String
    | RemovedFeedBack (Result Http.Error String)
    | TogglePause
    | BackToStations
    | Mdl (Material.Msg Msg)
    | Logout
    | NoOp
    | BackToPlaying
    | Mute
    | UnMute
    | RememberMe Bool
    | LoggedInRemember (Result Http.Error String)
    | SetSeekLocation Float
    | SetNewTime Float


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
                            let
                                log =
                                    Debug.log "Error logging in" "This broke"
                            in
                                model ! []

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
                            let
                                log =
                                    Debug.log "Error logging in" "This broke"
                            in
                                model ! []

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
                            let
                                log =
                                    Debug.log "Error fetching stations" error
                            in
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
                            let
                                error =
                                    Debug.log "Error starting station" "whoop"
                            in
                                model
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
                            let
                                error =
                                    Debug.log "Error starting station" "whoop"
                            in
                                model
                                    ! []

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
                        ! [ if List.length fields.songQueue == 1 then
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
                                    , currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ if List.length fields.songQueue == 1 then
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
                                    , currentTime = 0
                                    , isPlaying = True
                                }
                    }
                        ! [ Http.send SentFeedBack
                                (Feedback.send fields.authToken
                                    (currentSong model).trackToken
                                    False
                                )
                          , (if List.length fields.songQueue == 1 then
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

        BackToStations ->
            case model.state of
                LoggingIn fields ->
                    model ! []

                Playing fields ->
                    { model
                        | state =
                            Playing { fields | playingState = SelectingStation }
                    }
                        ! []

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
            { model
                | state =
                    LoggingIn
                        { email = ""
                        , password = ""
                        , remember = False
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



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


port togglePause : () -> Cmd msg


port replaySong : () -> Cmd msg


port audioLevel : Float -> Cmd msg


port rememberMe : String -> Cmd msg


port getProgressBarWidth : () -> Cmd msg


port sendProgressBarWidth : (Float -> msg) -> Sub msg


port sendNewTime : Float -> Cmd msg


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
        , Options.css "margin-top" "8px"
        , Options.css "z-index" "2"
        , Options.css "margin-left" "0px"
        , Options.css "visibility" visibleVal
        ]


viewTopMenu : Material.Model -> PlayingState -> Html Msg
viewTopMenu modelMdl playingState =
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
                [ (if playingState == SelectingStation then
                    Menu.item
                        [ Menu.onSelect BackToPlaying
                        , padding
                        , Menu.divider
                        ]
                        [ i "arrow_back", text "Back" ]
                   else
                    Menu.item
                        [ Menu.onSelect BackToStations
                        , padding
                        , Menu.divider
                        ]
                        [ i "apps", text "Stations" ]
                  )
                , Menu.item
                    [ Menu.onSelect Logout, padding ]
                    [ i "clear", text "Log out" ]
                ]
        ]


audioSelector : Float -> Bool -> Html Msg
audioSelector modelAudioLevel modelAudioHover =
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
    -> Html Msg
viewTopBar modelAudioLevel modelCurrentStationName modelAudioHover modelMdl playingState =
    div [ style [ ( "position", "relative" ) ] ]
        [ p
            [ style
                [ ( "text-align", "center" )
                , ( "position", "absolute" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "padding-top", "6px" )
                , ( "height", "10%" )
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
            , viewTopMenu modelMdl playingState
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
                    , ( "height", "5px" )
                    , ( "background-color", "black" )
                    ]
                , onClickXVal
                , id "progressBar"
                ]
                [ div
                    [ style
                        [ ( "height", "5px" )
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
                , div
                    [ style
                        [ ( "width", "100%" )
                        , ( "height", "100%" )
                        , ( "background-color", "#323842" )
                        , ( "display", "flex" )
                        , ( "justify-content", "center" )
                        , ( "align-items", "center" )
                        , (if fields.playingState == SelectingStation then
                            ( "cursor", "pointer" )
                           else
                            ( "", "" )
                          )
                        ]
                    , if
                        not
                            fields.isPlaying
                            && fields.playingState
                            == SelectingStation
                      then
                        onClick NoOp
                      else if fields.playingState == SelectingStation then
                        onClick BackToPlaying
                      else
                        onClick NoOp
                    ]
                    [ i
                        [ class "material-icons controlIcons"
                        , if fields.songQueue == [] then
                            onClick NoOp
                          else
                            onClick (SendThumbsDown stationId)
                        , style
                            [ ( "font-size", "20px" )
                            ]
                        ]
                        [ text "thumb_down_alt" ]
                    , i
                        [ class "material-icons controlIcons"
                        , if fields.songQueue == [] then
                            onClick NoOp
                          else
                            onClick ReplaySong
                        , style
                            [ ( "font-size", "26px" )
                            , ( "margin-left", "20px" )
                            ]
                        ]
                        [ text "replay" ]
                    , i
                        [ class "material-icons controlIcons"
                        , if fields.songQueue == [] then
                            onClick NoOp
                          else
                            onClick TogglePause
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
                    , i
                        [ class "material-icons controlIcons"
                        , style
                            [ ( "font-size", "26px" )
                            , ( "margin-right", "20px" )
                            ]
                        , if fields.songQueue == [] then
                            onClick NoOp
                          else
                            onClick (SkipSong stationId)
                        ]
                        [ text "skip_next" ]
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
                ]


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
                    , ( "margin-top", "2.75%" )
                    , ( "height", "60%" )
                    ]
                ]
                [ img
                    [ style
                        [ ( "-webkit-user-select", "none" )
                        , ( "height", "100%" )
                        ]
                    , src
                        (case (List.head fields.songQueue) of
                            Just song ->
                                song.albumArt

                            Nothing ->
                                "fillerIMG.jpg"
                        )
                    , style [ ( "border", "1px black solid" ) ]
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


viewLogin : Model -> Html Msg
viewLogin model =
    case model.state of
        LoggingIn fields ->
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
                        [ if fields.remember then
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

        Playing record ->
            text ""


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
                    , ( "-webkit-user-select", "none" )
                    ]
                , src url
                ]
                []
            , p [ style [ ( "text-align", "center" ) ] ] [ text name ]
            ]
        ]


viewStationSelector : Model -> Html Msg
viewStationSelector model =
    case model.state of
        LoggingIn fields ->
            div [] []

        Playing fields ->
            div
                [ style
                    [ ( "height", "100%" )
                    , ( "width", "100%" )
                    , ( "margin", "auto" )
                    , ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    ]
                ]
                [ viewTopBar fields.audioLevel
                    (getCurrentStation model).name
                    fields.audioHover
                    model.mdl
                    fields.playingState
                , Grid.grid
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
                        fields.stations
                    )
                , viewControls model (getCurrentStation model).id
                ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    case model.state of
        LoggingIn record ->
            div [] []

        Playing record ->
            div
                [ style
                    [ ( "height", "100%" )
                    , ( "width", "100%" )
                    , ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "margin", "auto" )
                    ]
                ]
                [ viewTopBar record.audioLevel
                    (case record.currentStation of
                        Just station ->
                            station.name

                        Nothing ->
                            ""
                    )
                    record.audioHover
                    model.mdl
                    record.playingState
                , viewSongInfo model
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
                , (case fields.playingState of
                    Normal ->
                        viewPlayer model

                    SelectingStation ->
                        viewStationSelector model
                  )
                ]

        LoggingIn fields ->
            viewLogin model
