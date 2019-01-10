module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Util.Types as Types
    exposing
        ( GlobalModel
        , Page(..)
        , LoginModel
        , cmds
        , Song
        )
import Util.Port as Port
import Util.Events as Events
import View.PreviousSongs as PreviousSongs
import View.Player as Player
import View.Ui as Ui
import Api.Login as AuthApi
import Api.Song as SongApi
import Api.Station as StationApi
import Page.Login
import Page.StationSelector
import Material
import Material.Button as Button
import Material.Options as Options
import Http
import Dict exposing (Dict)
import Time
import Keyboard
import Task


main : Program Flags GlobalModel Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { password : Maybe String
    , audioLevel : Maybe Float
    , username : Maybe String
    , email : Maybe String
    , newUser : Maybe String
    }


init : Flags -> ( GlobalModel, Cmd Msg )
init flags =
    let
        email =
            (case flags.email of
                Just email ->
                    email

                Nothing ->
                    ""
            )
    in
        case flags.password of
            Just password ->
                { page =
                    LoginWindow
                , authToken = ""
                , removingStationPopup = False
                , updatingStationPopup = False
                , updateStationNameInput = ""
                , username = flags.username
                , newUser =
                    if flags.newUser == Just "false" then
                        Just False
                    else
                        Just True
                , mdl = Material.model
                , keyPress = 0
                , seek = 0
                , isPlaying = False
                , audioLevel = flags.audioLevel
                , audioHover = False
                , currentTime = 0
                , songQueue = []
                , currentStation = Nothing
                , stations = []
                , previousSongs = []
                , error = ""
                , dominantColor = "FFFFFF"
                , loginModel =
                    { email = email
                    , password = password
                    , remember = False
                    , failed = False
                    }
                , selectorModel =
                    { state = Types.Selecting
                    , previousSongs = []
                    , searchResults = Dict.empty
                    , searchInput = ""
                    }
                , uiModel = {}
                }
                    ! [ Http.send (LoginMsg << Page.Login.LoggedInRemember)
                            (AuthApi.login
                                { email = email
                                , password = password
                                }
                            )
                      , Port.audioLevel
                            (case flags.audioLevel of
                                Just level ->
                                    level

                                Nothing ->
                                    1
                            )
                      ]

            Nothing ->
                { page =
                    LoginWindow
                , authToken = ""
                , removingStationPopup = False
                , updatingStationPopup = False
                , updateStationNameInput = ""
                , username = Nothing
                , newUser = Just True
                , mdl = Material.model
                , keyPress = 0
                , seek = 0
                , isPlaying = False
                , audioLevel = flags.audioLevel
                , audioHover = False
                , currentTime = 0
                , songQueue = []
                , currentStation = Nothing
                , stations = []
                , previousSongs = []
                , error = ""
                , dominantColor = "FFFFFF"
                , loginModel =
                    { email = email
                    , password = ""
                    , remember = False
                    , failed = False
                    }
                , selectorModel =
                    { state = Types.Selecting
                    , previousSongs = []
                    , searchResults = Dict.empty
                    , searchInput = ""
                    }
                , uiModel = {}
                }
                    ! [ Port.audioLevel
                            (case flags.audioLevel of
                                Just level ->
                                    level

                                Nothing ->
                                    1
                            )
                      ]


type Msg
    = -- PageMsg
      LoginMsg Page.Login.Msg
    | SelectorMsg Page.StationSelector.Msg
    | UiMsg Ui.Msg
      -- Other
    | SongEnded String
    | LoadedNextSongs (Result Http.Error (List Song))
    | Mdl (Material.Msg Msg)
    | KeyDown Int
    | SetCurrentTime Float
    | SetNewTime Float
    | PlayPreviousSong Song
    | NoOp
    | GotDetails (Result Http.Error String)
      -- Removing Stations
    | OpenRemoveStationPopup
    | CloseRemoveStationPopup
    | RemoveStation Types.Station
    | RemovedStation (Result Http.Error String)
      -- Updating Stations
    | UpdateStation
    | UpdatedStation (Result Http.Error String)
    | OpenUpdateStationPopup
    | CloseUpdateStationPopup
    | UpdateStationInput String


update : Msg -> GlobalModel -> ( GlobalModel, Cmd Msg )
update msg gm =
    case msg of
        LoginMsg msg ->
            let
                ( logModel, glModel, cmd ) =
                    Page.Login.update msg gm.loginModel gm
            in
                ( { glModel | loginModel = logModel }, Cmd.map LoginMsg cmd )

        SelectorMsg msg ->
            let
                ( selModel, glModel, cmd ) =
                    Page.StationSelector.update msg gm.selectorModel gm
            in
                ( { glModel | selectorModel = selModel }, Cmd.map SelectorMsg cmd )

        UiMsg msg ->
            let
                ( uiModel, glModel, cmd ) =
                    Ui.update msg gm.uiModel gm
            in
                ( { glModel | uiModel = uiModel }, Cmd.map UiMsg cmd )

        SongEnded stationId ->
            let
                currentSong =
                    (case List.head gm.songQueue of
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
                            , pandoraId = ""
                            , dominantColor = "FFFFFF"
                            }
                    )
            in
                ( { gm
                    | songQueue = (List.drop 1 gm.songQueue)
                    , previousSongs =
                        currentSong
                            :: gm.previousSongs
                    , currentTime = 0
                    , isPlaying = True
                  }
                , cmds
                    [ if List.length gm.songQueue == 2 then
                        Task.attempt LoadedNextSongs (SongApi.getNext stationId gm.authToken True)
                      else
                        Cmd.none
                    ]
                )

        LoadedNextSongs result ->
            case result of
                Ok songs ->
                    { gm
                        | songQueue =
                            List.append
                                gm.songQueue
                                songs
                    }
                        ! []

                Err error ->
                    let
                        stu =
                            case error of
                                Http.BadUrl str ->
                                    "Bad url " ++ str

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"

                                Http.BadStatus resp ->
                                    "Bad status :" ++ resp.body

                                Http.BadPayload str resp ->
                                    "Bad payload: " ++ str
                    in
                        gm ! []

        Mdl msg ->
            Material.update Mdl msg gm

        KeyDown code ->
            case gm.page of
                LoginWindow ->
                    ( gm
                    , cmds
                        (if code == 13 then
                            (if gm.loginModel.remember then
                                [ Http.send (LoginMsg << Page.Login.LoggedInRemember)
                                    (AuthApi.login
                                        { email = gm.loginModel.email
                                        , password = gm.loginModel.password
                                        }
                                    )
                                , Port.rememberEmail gm.loginModel.email
                                , Port.rememberPassword gm.loginModel.password
                                ]
                             else
                                [ Http.send (LoginMsg << Page.Login.LoggedIn)
                                    (AuthApi.login
                                        { email = gm.loginModel.email
                                        , password = gm.loginModel.password
                                        }
                                    )
                                ]
                            )
                         else
                            []
                        )
                    )

                _ ->
                    (if code == 32 then
                        { gm
                            | isPlaying = not gm.isPlaying
                            , keyPress = code
                        }
                     else
                        { gm | isPlaying = gm.isPlaying }
                    )
                        ! [ (if code == 32 then
                                Port.togglePause ()
                             else
                                Cmd.none
                            )
                          ]

        SetNewTime totalWidth ->
            case List.head gm.songQueue of
                Just currentSong ->
                    let
                        newTime =
                            (gm.seek
                                / totalWidth
                            )
                                * (toFloat currentSong.trackLength)
                    in
                        { gm | currentTime = newTime }
                            ! [ Port.sendNewTime newTime ]

                Nothing ->
                    gm ! []

        SetCurrentTime _ ->
            ( { gm | currentTime = gm.currentTime + 1 }, cmds [] )

        PlayPreviousSong song ->
            ( { gm
                | songQueue = song :: gm.songQueue
                , page = Player
                , currentTime = 0
                , isPlaying = True
              }
            , cmds []
            )

        NoOp ->
            gm ! []

        RemoveStation removedStation ->
            { gm
                | stations =
                    (List.filter
                        (\station -> station.id /= removedStation.id)
                        gm.stations
                    )
            }
                ! [ Http.send RemovedStation (StationApi.remove removedStation.id gm.authToken) ]

        RemovedStation _ ->
            { gm
                | page = StationSelector
                , removingStationPopup = False
                , isPlaying = False
                , currentTime = 0
                , currentStation = Nothing
            }
                ! []

        OpenRemoveStationPopup ->
            { gm | removingStationPopup = True } ! []

        CloseRemoveStationPopup ->
            { gm | removingStationPopup = False } ! []

        OpenUpdateStationPopup ->
            { gm | updatingStationPopup = True } ! []

        CloseUpdateStationPopup ->
            { gm | updatingStationPopup = False } ! []

        UpdateStation ->
            let
                stationId =
                    case gm.currentStation of
                        Just station ->
                            station.id

                        Nothing ->
                            ""
            in
                gm ! [ Http.send UpdatedStation (StationApi.update stationId gm.updateStationNameInput gm.authToken) ]

        UpdatedStation result ->
            case result of
                Ok result ->
                    let
                        station =
                            case gm.currentStation of
                                Just station ->
                                    station

                                Nothing ->
                                    { id = ""
                                    , name = ""
                                    , art = ""
                                    }
                    in
                        { gm
                            | currentStation =
                                Just
                                    { id = station.id
                                    , name = gm.updateStationNameInput
                                    , art = station.art
                                    }
                            , updatingStationPopup = False
                        }
                            ! []

                Err error ->
                    let
                        log =
                            Debug.log "Error updating station" error
                    in
                        gm ! []

        UpdateStationInput input ->
            { gm | updateStationNameInput = input } ! []

        GotDetails result ->
            case result of
                Ok color ->
                    { gm | dominantColor = color } ! []

                Err error ->
                    { gm | dominantColor = "FFFFFF" } ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : GlobalModel -> Sub Msg
subscriptions gm =
    Sub.batch
        [ if gm.isPlaying then
            Time.every Time.second SetCurrentTime
          else
            Sub.none
        , Material.subscriptions Mdl gm
        , Port.sendProgressBarWidth SetNewTime
        , Keyboard.downs KeyDown
        ]



-- ΩΩΩ VIEW ΩΩΩ


updatingPopup :
    GlobalModel
    -> Html Msg
updatingPopup gm =
    let
        station =
            case gm.currentStation of
                Just station ->
                    station

                Nothing ->
                    { id = ""
                    , name = ""
                    , art = ""
                    }
    in
        div
            [ style
                [ ( "height", "350px" )
                , ( "width", "400px" )
                , ( "z-index", "5" )
                , ( "border-bottom", "1px black solid" )
                , ( "border-right", "1px black solid" )
                , ( "border-left", "1px black solid" )
                , ( "align-self", "center" )
                , ( "position", "absolute" )
                , ( "background-color", "white" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            ]
            [ h5
                [ style
                    [ ( "text-align", "center" )
                    , ( "margin-top", "10%" )
                    ]
                ]
                [ text "What would you like to rename" ]
            , h5 [ style [ ( "text-align", "center" ) ] ] [ text ("\"" ++ station.name ++ "\" to?") ]
            , input
                [ style
                    [ ( "align-self", "center" )
                    , ( "margin-top", "5%" )
                    , ( "height", "10%" )
                    , ( "width", "60%" )
                    ]
                , onInput UpdateStationInput
                , Events.blockSpacebar NoOp
                ]
                []
            , div
                [ style
                    [ ( "margin-top", "7.5%" )
                    , ( "align-self", "center" )
                    , ( "display", "flex" )
                    , ( "justify-content", "space-around" )
                    , ( "width", "100%" )
                    ]
                ]
                [ Button.render Mdl
                    [ 987 ]
                    gm.mdl
                    [ Options.onClick CloseUpdateStationPopup
                    , Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.css "height" "75px"
                    , Options.css "width" "125px"
                    ]
                    [ text "Cancel" ]
                , Button.render Mdl
                    [ 573 ]
                    gm.mdl
                    [ Options.onClick UpdateStation
                    , Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.css "height" "75px"
                    , Options.css "width" "125px"
                    ]
                    [ text "Update" ]
                ]
            ]


removingPopup :
    GlobalModel
    -> Html Msg
removingPopup gm =
    let
        station =
            case gm.currentStation of
                Just station ->
                    station

                Nothing ->
                    { id = ""
                    , name = ""
                    , art = ""
                    }
    in
        div
            [ style
                [ ( "height", "350px" )
                , ( "width", "400px" )
                , ( "z-index", "5" )
                , ( "border-bottom", "1px black solid" )
                , ( "border-right", "1px black solid" )
                , ( "border-left", "1px black solid" )
                , ( "align-self", "center" )
                , ( "position", "absolute" )
                , ( "background-color", "white" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            ]
            [ h5
                [ style
                    [ ( "text-align", "center" )
                    , ( "margin-top", "15%" )
                    ]
                ]
                [ text "Are you sure you would like to delete" ]
            , h5 [ style [ ( "text-align", "center" ) ] ]
                [ text ("\"" ++ station.name ++ "\"?") ]
            , div
                [ style
                    [ ( "margin-top", "17.5%" )
                    , ( "align-self", "center" )
                    , ( "display", "flex" )
                    , ( "justify-content", "space-around" )
                    , ( "width", "100%" )
                    ]
                ]
                [ Button.render Mdl
                    [ 987 ]
                    gm.mdl
                    [ Options.onClick CloseRemoveStationPopup
                    , Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.css "height" "75px"
                    , Options.css "width" "125px"
                    ]
                    [ text "Cancel" ]
                , Button.render Mdl
                    [ 573 ]
                    gm.mdl
                    [ Options.onClick (RemoveStation station)
                    , Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.css "height" "75px"
                    , Options.css "width" "125px"
                    ]
                    [ text "Delete" ]
                ]
            ]


onEnded : String -> Attribute Msg
onEnded stationId =
    Html.Events.on "ended"
        (Decode.succeed (SongEnded stationId))


viewPlayer : GlobalModel -> Html Msg -> Html Msg
viewPlayer gm content =
    let
        station =
            case gm.currentStation of
                Just station ->
                    station

                Nothing ->
                    { id = ""
                    , name = ""
                    , art = ""
                    }
    in
        div
            [ style
                [ ( "height", "100%" )
                , ( "width", "100%" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "margin", "auto" )
                , ( "background-color", "rgba(255, 255, 255, 0)" )
                , ( "z-index", "1" )
                ]
            ]
            [ Html.map UiMsg (Ui.viewTopBar gm)
            , content
            , Html.map UiMsg (Ui.viewControls gm)
            , (if gm.removingStationPopup then
                removingPopup gm
               else
                text ""
              )
            , (if gm.updatingStationPopup then
                updatingPopup gm
               else
                text ""
              )
            ]


view : GlobalModel -> Html Msg
view gm =
    let
        content =
            case gm.page of
                LoginWindow ->
                    Html.map LoginMsg (Page.Login.view gm)

                Player ->
                    Player.viewSongInfo gm

                StationSelector ->
                    Html.map SelectorMsg (Page.StationSelector.view gm)

                ChatWindow ->
                    text ""

                PreviousSongs ->
                    PreviousSongs.view gm.previousSongs PlayPreviousSong gm

        song =
            audio
                [ onEnded
                    (case (gm.currentStation) of
                        Just station ->
                            station.id

                        Nothing ->
                            ""
                    )
                , id "songAudio"
                , src
                    (case (List.head gm.songQueue) of
                        Just song ->
                            song.audioURL

                        Nothing ->
                            ""
                    )
                , autoplay True
                ]
                []

        backgroundImg =
            img
                [ style
                    [ ( "filter", "blur(5px)" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "absolute" )
                    , ( "transform", "scale(10)" )
                    , ( "justify-self", "center" )
                    , ( "align-self", "center" )
                    , ( "overflow", "hidden" )
                    ]
                , src
                    (case List.head gm.songQueue of
                        Just song ->
                            song.albumArt

                        Nothing ->
                            ""
                    )
                ]
                []

        backgroundColor =
            div
                [ style
                    [ ( "background-color"
                      , "#"
                            ++ (case List.head gm.songQueue of
                                    Just song ->
                                        song.dominantColor

                                    Nothing ->
                                        "FFFFFF"
                               )
                      )
                    , ( "opacity", ".75" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "absolute" )
                    ]
                ]
                []
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "overflow", "hidden" )
                , ( "position", "relative" )
                ]
            ]
            [ backgroundImg, backgroundColor, song, viewPlayer gm content ]
