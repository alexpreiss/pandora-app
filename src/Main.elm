module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import Util.Types
    exposing
        ( GlobalModel
        , Page(..)
        , LoginModel
        , cmds
        , Song
        )
import Util.Port as Port
import View.Player as Player
import View.Ui as Ui
import Api.Login as AuthApi
import Api.Song as SongApi
import Page.Login
import Page.StationSelector
import Material
import Http
import Dict exposing (Dict)
import Time
import Keyboard


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
                , deletingStationPopup = False
                , updatingStationPopup = False
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
                , loginModel =
                    { email = email
                    , password = password
                    , remember = False
                    , failed = False
                    }
                , selectorModel =
                    { state = Util.Types.Selecting
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
                      ]

            Nothing ->
                { page =
                    LoginWindow
                , authToken = ""
                , deletingStationPopup = False
                , updatingStationPopup = False
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
                , loginModel =
                    { email = email
                    , password = ""
                    , remember = False
                    , failed = False
                    }
                , selectorModel =
                    { state = Util.Types.Selecting
                    , previousSongs = []
                    , searchResults = Dict.empty
                    , searchInput = ""
                    }
                , uiModel = {}
                }
                    ! []


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
            ( { gm
                | songQueue = (List.drop 1 gm.songQueue)
                , previousSongs =
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
                            }
                    )
                        :: gm.previousSongs
                , currentTime = 0
                , isPlaying = True
              }
            , cmds
                [ if List.length gm.songQueue == 2 then
                    Http.send LoadedNextSongs
                        (SongApi.getNext stationId gm.authToken False)
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


onEnded : String -> Attribute Msg
onEnded stationId =
    Html.Events.on "ended"
        (Decode.succeed (SongEnded stationId))


viewPlayer : GlobalModel -> Html Msg -> Html Msg
viewPlayer gm content =
    div
        [ style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "margin", "auto" )
            ]
        ]
        [ Html.map UiMsg (Ui.viewTopBar gm)
        , content
        , Html.map UiMsg (Ui.viewControls gm)

        -- , (if removingStationPopup then
        --     Station.removingPopup mdl
        --         Mdl
        --         (case currentStation of
        --             Just station ->
        --                 station
        --
        --             Nothing ->
        --                 { id = ""
        --                 , name = ""
        --                 , art = ""
        --                 }
        --         )
        --         CloseRemoveStationPopup
        --         RemoveStation
        --    else
        --     text ""
        --   )
        -- , (if gm.updatingStationPopup then
        --     Station.updatingPopup mdl
        --         Mdl
        --         (case currentStation of
        --             Just station ->
        --                 station
        --
        --             Nothing ->
        --                 { id = ""
        --                 , name = ""
        --                 , art = ""
        --                 }
        --         )
        --         CloseUpdateStationPopup
        --         UpdateStation
        --         NoOp
        --         NameInputToModel
        --    else
        --     text ""
        --   )
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
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                , ( "overflow-x", "hidden" )
                ]
            ]
            [ song, viewPlayer gm content ]
