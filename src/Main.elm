port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (Request)
import Material.Slider as Slider
import Material.Options as Options
import Time


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- ΩΩΩ MODEL ΩΩΩ


currentSong : Model -> Song
currentSong model =
    case (List.head model.songQueue) of
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


type State
    = LoggingIn
    | SelectingStation
    | Paused
    | Playing


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


type alias Model =
    { email : String
    , password : String
    , authToken : String
    , stations : List Station
    , currentStation : Station
    , songQueue : List Song
    , currentTime : Float
    , isPlaying : Bool
    , previousSongs : List Song
    , audioLevel : Float
    , audioHover : Bool
    , state : State
    }


init : ( Model, Cmd Msg )
init =
    { email = ""
    , password = ""
    , authToken = ""
    , stations = []
    , currentStation =
        { id = ""
        , name = ""
        , art = ""
        }
    , songQueue = []
    , currentTime = 0
    , isPlaying = True
    , previousSongs = []
    , audioLevel = 1
    , audioHover = False
    , state = LoggingIn
    }
        ! []



-- ΩΩΩ UPDATE ΩΩΩ


authDecode : Decode.Decoder String
authDecode =
    (Decode.at [ "authToken" ] Decode.string)


fragmentListDecoder : Decode.Decoder (List Song)
fragmentListDecoder =
    (Decode.at [ "tracks" ]
        (Decode.list
            fragmentDecoder
        )
    )


fragmentDecoder : Decode.Decoder Song
fragmentDecoder =
    Decode.map8 Song
        (Decode.field "songTitle" Decode.string)
        (Decode.field "trackLength" Decode.int)
        (Decode.field "rating" Decode.int)
        (Decode.field "audioURL" Decode.string)
        (Decode.field "artistName" Decode.string)
        (Decode.field "albumTitle" Decode.string)
        (Decode.at [ "albumArt", "4", "url" ] Decode.string)
        (Decode.field "trackToken" Decode.string)


stationDecoder : Decode.Decoder Station
stationDecoder =
    -- Decode.list (Decode.at [ "stations" ])
    Decode.map3 Station
        (Decode.field "stationId" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.at [ "art", "4", "url" ] Decode.string)


stationListDecoder : Decode.Decoder (List Station)
stationListDecoder =
    (Decode.at [ "stations" ]
        (Decode.list
            stationDecoder
        )
    )


sendFeedback : String -> String -> Bool -> Request String
sendFeedback authToken songId isPositive =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/station/addFeedback"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isPositive", (Encode.bool isPositive) )
                    , ( "trackToken", (Encode.string songId) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }


getNextSongs : String -> String -> Bool -> Request (List Song)
getNextSongs stationId authToken isStationStart =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/playlist/getFragment"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "stationId", (Encode.string stationId) )
                    , ( "isStationStart", (Encode.bool isStationStart) )
                    , ( "fragmentRequestReason", (Encode.string "Normal") )
                    , ( "audioFormat", (Encode.string "mp3-hifi") )
                    , ( "startingAtTrackId", Encode.null )
                    , ( "onDemandArtistMessageArtistUidHex", Encode.null )
                    , ( "onDemandArtistMessageIdHex", Encode.null )
                    ]
                )
        , expect = Http.expectJson fragmentListDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getStations : String -> Request (List Station)
getStations authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/station/getStations"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "pageSize", (Encode.int 250) )
                    ]
                )
        , expect = Http.expectJson stationListDecoder
        , timeout = Nothing
        , withCredentials = False
        }


login : Model -> Request String
login model =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/auth/login"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "username", (Encode.string model.email) )
                    , ( "password", (Encode.string model.password) )
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
    | LoggedIn (Result Http.Error String)
    | GotStations (Result Http.Error (List Station))
    | StartStation String String String
    | LoadNextSongs String
    | LoadedNextSongs (Result Http.Error (List Song))
    | StartedStation (Result Http.Error (List Song))
    | SongEnded String
    | SkipSong String
    | SetCurrentTime Float
    | TogglePause
    | ReplaySong
    | SendThumbsDown String
    | SendThumbsUp String
    | SentFeedBack (Result Http.Error String)
    | SetAudio Float
    | HoveringAudio
    | UnHoveringAudio


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail input ->
            { model | email = input } ! []

        InputPassword input ->
            { model | password = input } ! []

        Login ->
            model ! [ Http.send LoggedIn (login model) ]

        LoggedIn result ->
            case result of
                Ok token ->
                    { model | authToken = token, state = SelectingStation }
                        ! [ Http.send GotStations (getStations token) ]

                Err error ->
                    let
                        log =
                            Debug.log "Error logging in" "This broke"
                    in
                        model ! []

        GotStations result ->
            case result of
                Ok stations ->
                    { model | stations = stations } ! []

                Err error ->
                    let
                        log =
                            Debug.log "Error fetching stations" "This broke"
                    in
                        model ! []

        StartStation id name art ->
            { model
                | currentStation =
                    { id = id
                    , name = name
                    , art = art
                    }
                , songQueue = []
            }
                ! [ Http.send StartedStation (getNextSongs id model.authToken True) ]

        LoadNextSongs id ->
            model
                ! [ Http.send LoadedNextSongs (getNextSongs id model.authToken False) ]

        StartedStation result ->
            case result of
                Ok songs ->
                    { model
                        | songQueue = List.append model.songQueue songs
                        , currentTime = 0
                        , state = Playing
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
            case result of
                Ok songs ->
                    { model
                        | songQueue = List.append model.songQueue songs
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
            { model
                | songQueue = (List.drop 1 model.songQueue)
                , previousSongs =
                    currentSong model :: model.previousSongs
                , currentTime = 0
                , isPlaying = True
            }
                ! [ if List.length model.songQueue == 1 then
                        Http.send LoadedNextSongs (getNextSongs id model.authToken False)
                    else
                        Cmd.none
                  ]

        SkipSong id ->
            { model
                | songQueue = (List.drop 1 model.songQueue)
                , currentTime = 0
                , isPlaying = True
            }
                ! [ if List.length model.songQueue == 1 then
                        Http.send LoadedNextSongs (getNextSongs id model.authToken False)
                    else
                        Cmd.none
                  ]

        SetCurrentTime _ ->
            { model | currentTime = model.currentTime + 1 } ! []

        TogglePause ->
            { model | isPlaying = not model.isPlaying } ! [ togglePause () ]

        ReplaySong ->
            { model | currentTime = 0 } ! [ replaySong () ]

        SendThumbsDown id ->
            { model
                | songQueue = (List.drop 1 model.songQueue)
                , currentTime = 0
                , isPlaying = True
            }
                ! [ Http.send SentFeedBack (sendFeedback model.authToken (currentSong model).trackToken False)
                  , (if List.length model.songQueue == 1 then
                        Http.send LoadedNextSongs (getNextSongs id model.authToken False)
                     else
                        Cmd.none
                    )
                  ]

        SendThumbsUp id ->
            model
                ! [ (case (List.head model.songQueue) of
                        Just song ->
                            Http.send SentFeedBack (sendFeedback model.authToken song.trackToken False)

                        Nothing ->
                            Cmd.none
                    )
                  , (if List.length model.songQueue == 1 then
                        Http.send LoadedNextSongs (getNextSongs id model.authToken True)
                     else
                        Cmd.none
                    )
                  ]

        SentFeedBack _ ->
            model ! []

        SetAudio level ->
            { model | audioLevel = level } ! [ audioLevel level ]

        HoveringAudio ->
            { model | audioHover = True } ! []

        UnHoveringAudio ->
            { model | audioHover = False } ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


port togglePause : () -> Cmd msg


port replaySong : () -> Cmd msg


port audioLevel : Float -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.isPlaying then
            Time.every Time.second SetCurrentTime
          else
            Sub.none
        ]



-- ΩΩΩ VIEW ΩΩΩ


onEnded : String -> Attribute Msg
onEnded stationId =
    Html.Events.on "ended" (Decode.succeed (SongEnded stationId))


onHover : Attribute Msg
onHover =
    Html.Events.on "mouseover" (Decode.succeed HoveringAudio)


onUnHover : Attribute Msg
onUnHover =
    Html.Events.on "mouseout" (Decode.succeed UnHoveringAudio)


audioSlider : Model -> String -> Html Msg
audioSlider model visibleVal =
    Slider.view
        [ Slider.onChange SetAudio
        , Slider.max 1
        , Slider.min 0
        , Slider.step 0.1
        , Slider.value model.audioLevel
        , Options.css "width" "100px"
        , Options.css "margin-top" "8px"
        , Options.css "z-index" "2"
        , Options.css "margin-left" "0px"
        , Options.css "visibility" visibleVal
        ]


viewTopBar : Model -> Html Msg
viewTopBar model =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "35px" )
            , ( "background-color", "rgba(242, 242, 242, 0)" )
            , ( "display", "flex" )
            ]
        ]
        [ div
            [ onHover
            , onUnHover
            , style
                [ ( "display", "flex" )
                ]
            ]
            [ i
                [ style
                    [ ( "margin-right", "auto" )
                    , ( "margin-left", "10px" )
                    , ( "margin-top", "5px" )
                    , ( "z-index", "3" )
                    ]
                , class "material-icons topIcons"
                ]
                [ text
                    (if model.audioLevel > 0.5 then
                        "volume_up"
                     else
                        "volume_down"
                    )
                ]
            , if model.audioHover then
                audioSlider model "visible"
              else
                audioSlider model "hidden"
            ]
        , p [ style [ ( "margin", "auto" ) ] ] [ text model.currentStation.name ]
        , i
            [ style
                [ ( "margin-left", "auto" )
                , ( "margin-right", "10px" )
                , ( "margin-top", "5px" )
                ]
            , class "material-icons topIcons"
            ]
            [ text "keyboard_arrow_down" ]
        ]


timePercentage : Model -> Float
timePercentage model =
    100
        * (model.currentTime
            / case (List.head model.songQueue) of
                Just song ->
                    toFloat song.trackLength

                Nothing ->
                    1.0
          )


viewProgressBar : Model -> Html Msg
viewProgressBar model =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "5px" )
            , ( "background-color", "#323842" )
            , ( "margin-top", "auto" )
            , ( "border-top", "black solid 1px" )
            ]
        ]
        [ div
            [ style
                [ ( "height", "5px" )
                , ( "width", ((toString (timePercentage model)) ++ "%") )
                , ( "background-color", "white" )
                , ( "border-right", "black solid 1px" )
                ]
            ]
            []
        ]


viewControls : Model -> String -> Html Msg
viewControls model stationId =
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "60px" )
            , ( "background-color", "#323842" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "center" )
            ]
        ]
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
                (if model.isPlaying then
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
            , onClick (SkipSong stationId)
            ]
            [ text "skip_next" ]
        , i
            [ class "material-icons controlIcons"
            , onClick (SendThumbsUp stationId)
            , style
                [ ( "font-size", "20px" )
                ]
            ]
            [ text "thumb_up_alt" ]
        ]


viewSongInfo : Model -> Html Msg
viewSongInfo model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "margin-top", "2.75%" )
            ]
        ]
        [ img
            [ height 335
            , width 335
            , src
                (case (List.head model.songQueue) of
                    Just song ->
                        song.albumArt

                    Nothing ->
                        "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAMAAAAJbSJIAAAAOVBMVEX07/PJycnz8PHj4uLt6u318PPy8PHU1NTHycjc3dzQz9Dz7vLc2tvGxsbLy8vz8fTp5ebm5ubi4+L4IaaGAAACgElEQVR4nO3b226jMBRAUS7xCQm2If3/jx2bpsNVARVGcDx7PfSlUGXrGAiIZhkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP+SuR3JnJ2zwFRVfpSqumJhcVhfTCzOzllwaGF+3cJqt4sX+vt+/tKFz8LsJPK8euHeP3P5wr0fjcIT/S007eO3WqOisCndLy+CrmxUFJodhTpm+P8VOpdv/E6us9B5W7al9ckWOitNljWNPDYsXZWFrTGhsM7MlqNTYaF79F/ixK4mKiz0cYn+/L5ePRb1FVa2GWxgbHqF7jXaYvVIVF/YJl9Yrl34FRba4f1wisdhOJf2n1RkfC71djZShYVucDKV8QjDl50UZpjn5XuKIl+T80w7/wqgsjC38elbZmY5vpBbEjOMx1v5CvcW03lZmZ96lBZ2N4gzrg2DnQ5RbeGSKt5vTJduUoW222UyxJQKXdvtMzkSUyr00u1jZLRpSoX2p/CZaGE8k3ZMmWihr+v3XqNlmk7h4J7DDJdpKoWuuvcvk4wfPSZQ6Jy3r2Zw1zg6myZQ6G1bFDJ8PFUMz6a6C7vp1c18vzKNQhenZxY/9i2JwnCvvzC9b+aeQKGzHz7w8KGH1sJw+as/7NcvU62FYYl+KjT9RV9p4ecJRv3zVZWFIXDtNSm5O8WFYYmuTDBe9d+JGgs3TDB6JyosDJeJenWEWXxe3CWqKWyf7zdFnzbb+qqidNsreeurECl+bJpg1O0Tfqgo3PlnKDzPd6Hsd+1CX+537TfZw43ubvm1C49C4RmKAxZo74qFcsBJpidn5yzY+88yE2fnAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAS9weVjTfmgUbdUgAAAABJRU5ErkJggg=="
                )
            , style [ ( "border", "1px black solid" ) ]
            ]
            []
        , p
            [ style
                [ ( "font-size", "16px" )
                , ( "margin-top", "15px" )
                , ( "margin-bottom", "0px" )
                ]
            ]
            [ text
                (case (List.head model.songQueue) of
                    Just song ->
                        song.songTitle

                    Nothing ->
                        "Loading..."
                )
            ]
        , p
            [ style
                [ ( "font-size", "14px" )
                , ( "margin-top", "5px" )
                ]
            ]
            [ text
                (case (List.head model.songQueue) of
                    Just song ->
                        song.artistName ++ " - " ++ song.albumTitle

                    Nothing ->
                        ""
                )
            ]
        ]


viewLogin : Model -> Html Msg
viewLogin model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "width", "650px" )
            , ( "margin", "auto" )
            , ( "margin-top", "10px" )
            ]
        ]
        [ input
            [ style [ ( "margin-bottom", "10px" ) ]
            , type_ "email"
            , placeholder "Email"
            , onInput InputEmail
            ]
            []
        , input
            [ type_ "password"
            , placeholder "Password"
            , onInput InputPassword
            ]
            []
        , button
            [ style
                [ ( "margin-top", "10px" )
                , ( "height", "40px" )
                ]
            , onClick Login
            ]
            [ text "Log in" ]
        ]


viewStation : Model -> String -> String -> String -> Html Msg
viewStation model id name url =
    div
        [ style
            [ ( "width", "150px" )
            , ( "height", "175px" )
            ]
        , onClick (StartStation id name url)
        ]
        [ img
            [ style
                [ ( "height", "150px" )
                , ( "width", "150px" )
                , ( "border", "1px solid black" )
                ]
            , src url
            ]
            []
        , p [ style [ ( "text-align", "center" ) ] ] [ text name ]
        ]


viewStations : Model -> Html Msg
viewStations model =
    div [ style [ ( "display", "flex" ) ] ]
        (List.map
            (\station ->
                viewStation model
                    station.id
                    station.name
                    station.art
            )
            model.stations
        )


viewSong : Song -> Html Msg
viewSong song =
    span [] [ text song.artistName, text " ", text song.songTitle ]


view : Model -> Html Msg
view model =
    case model.state of
        LoggingIn ->
            viewLogin model

        SelectingStation ->
            viewStations model

        Paused ->
            div []
                [ audio
                    [ onEnded model.currentStation.id
                    , id "songAudio"
                    , src
                        (case (List.head model.songQueue) of
                            Just song ->
                                song.audioURL

                            Nothing ->
                                ""
                        )
                    , autoplay True
                    ]
                    []
                , div
                    [ class "view"
                    , style
                        [ ( "background-color", "#ffffff" )
                        , ( "background-size", "cover" )
                        , ( "height", "550px" )
                        , ( "width", "650px" )
                        , ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "border", "black 1px solid" )
                        , ( "margin", "auto" )
                        , ( "margin-top", "100px" )
                        ]
                    ]
                    [ viewTopBar model
                    , viewSongInfo model
                    , viewProgressBar model
                    , viewControls model model.currentStation.id
                    ]
                , div
                    [ style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        ]
                    ]
                    (List.map viewSong model.songQueue)
                , p [] [ text (toString model.audioLevel) ]
                ]

        Playing ->
            div []
                [ audio
                    [ onEnded model.currentStation.id
                    , id "songAudio"
                    , src
                        (case (List.head model.songQueue) of
                            Just song ->
                                song.audioURL

                            Nothing ->
                                ""
                        )
                    , autoplay True
                    ]
                    []
                , div
                    [ class "view"
                    , style
                        [ ( "background-color", "#ffffff" )
                        , ( "background-size", "cover" )
                        , ( "height", "550px" )
                        , ( "width", "650px" )
                        , ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "border", "black 1px solid" )
                        , ( "margin", "auto" )
                        , ( "margin-top", "100px" )
                        ]
                    ]
                    [ viewTopBar model
                    , viewSongInfo model
                    , viewProgressBar model
                    , viewControls model model.currentStation.id
                    ]
                , div
                    [ style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        ]
                    ]
                    (List.map viewSong model.songQueue)
                , p [] [ text (toString model.audioLevel) ]
                ]
