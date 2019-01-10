module View.Ui exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Util.Types as Types exposing (GlobalModel, cmds, Song, Page(..), UiModel)
import Util.Port as Port
import Material
import Material.Options as Options
import Material.Tooltip as Tooltip
import Material.Icon as Icon
import Material.Menu as Menu
import Material.Slider as Slider
import Api.Controller as Api
import Api.Song as SongApi
import Http
import Json.Decode as Decode
import Task


-- ΩΩΩ MODEL ΩΩΩ


onClickXVal : Attribute Msg
onClickXVal =
    Html.Events.on "click"
        (Decode.map
            SetSeekLocation
            (Decode.field "clientX" Decode.float)
        )


init : ( UiModel, Cmd Msg )
init =
    {}
        ! []



-- ΩΩΩ UPDATE ΩΩΩ


type Msg
    = SendThumbsUp String
    | SendThumbsDown String
    | SentFeedBack (Result Http.Error String)
    | SkipSong String
    | NoOp
    | Mdl (Material.Msg Msg)
    | LoadedNextSongs (Result Http.Error (List Song))
    | TogglePause
    | ReplaySong
    | RemoveThumbsUp String
    | RemovedFeedBack (Result Http.Error String)
    | ToStations
    | ToChat
    | ToPreviousSongs
    | ToPlaying
    | Logout
    | OpenDeleteStationPopup
    | OpenUpdateStationPopup
    | HoveringAudio
    | UnHoveringAudio
    | SetAudio Float
    | UnMute
    | Mute
    | SetSeekLocation Float
    | GotDetails (Result Http.Error String)


update : Msg -> UiModel -> GlobalModel -> ( UiModel, GlobalModel, Cmd Msg )
update msg model gm =
    let
        currentSong =
            List.head gm.songQueue
    in
        case msg of
            SendThumbsUp id ->
                let
                    song =
                        List.head gm.songQueue

                    updatedSong =
                        case song of
                            Just song ->
                                Just { song | rating = 1 }

                            Nothing ->
                                Nothing

                    updatedModel =
                        { gm
                            | songQueue =
                                case updatedSong of
                                    Just song ->
                                        song
                                            :: (List.drop
                                                    1
                                                    gm.songQueue
                                               )

                                    Nothing ->
                                        gm.songQueue
                        }
                in
                    ( model
                    , updatedModel
                    , cmds
                        [ (case (List.head gm.songQueue) of
                            Just song ->
                                Http.send SentFeedBack
                                    (Api.sendFb
                                        gm.authToken
                                        song.trackToken
                                        True
                                    )

                            Nothing ->
                                Cmd.none
                          )
                        ]
                    )

            SendThumbsDown id ->
                let
                    trackToken =
                        case currentSong of
                            Just song ->
                                song.trackToken

                            Nothing ->
                                ""
                in
                    ( model
                    , { gm
                        | songQueue = (List.drop 1 gm.songQueue)
                        , previousSongs =
                            case currentSong of
                                Just song ->
                                    song :: gm.previousSongs

                                Nothing ->
                                    gm.previousSongs
                        , currentTime = 0
                        , isPlaying = True
                      }
                    , cmds
                        [ Http.send SentFeedBack
                            (Api.sendFb gm.authToken
                                trackToken
                                False
                            )
                        , (if List.length gm.songQueue == 2 then
                            Http.send LoadedNextSongs
                                (Api.getFragment id gm.authToken False)
                           else
                            Cmd.none
                          )
                        ]
                    )

            LoadedNextSongs result ->
                case result of
                    Ok songs ->
                        ( model
                        , { gm
                            | songQueue =
                                List.append
                                    gm.songQueue
                                    songs
                          }
                        , cmds []
                        )

                    Err error ->
                        ( model, gm, cmds [] )

            SentFeedBack _ ->
                ( model, gm, cmds [] )

            SkipSong id ->
                ( model
                , { gm
                    | songQueue = (List.drop 1 gm.songQueue)
                    , previousSongs =
                        case currentSong of
                            Just song ->
                                song :: gm.previousSongs

                            Nothing ->
                                gm.previousSongs
                    , currentTime = 0
                    , isPlaying = True
                  }
                , cmds
                    [ if List.length gm.songQueue == 2 then
                        Http.send LoadedNextSongs
                            (Api.getFragment id gm.authToken False)
                      else
                        Cmd.none
                    , Task.attempt LoadedNextSongs (SongApi.getNext id gm.authToken True)
                    ]
                )

            NoOp ->
                ( model, gm, cmds [] )

            Mdl msg ->
                let
                    ( glModel, cmd ) =
                        Material.update Mdl msg gm
                in
                    ( model, glModel, cmd )

            TogglePause ->
                ( model
                , { gm | isPlaying = not gm.isPlaying }
                , cmds
                    [ Port.togglePause () ]
                )

            ReplaySong ->
                ( model
                , { gm
                    | currentTime = 0
                    , isPlaying = True
                  }
                , cmds
                    [ Port.replaySong () ]
                )

            RemoveThumbsUp id ->
                let
                    song =
                        List.head gm.songQueue

                    updatedSong =
                        case song of
                            Just song ->
                                Just { song | rating = 0 }

                            Nothing ->
                                Nothing

                    updatedModel =
                        { gm
                            | songQueue =
                                case updatedSong of
                                    Just song ->
                                        song
                                            :: (List.drop 1 gm.songQueue)

                                    Nothing ->
                                        gm.songQueue
                        }
                in
                    ( model
                    , updatedModel
                    , cmds
                        [ (case (List.head gm.songQueue) of
                            Just song ->
                                Http.send RemovedFeedBack
                                    (Api.removeFb
                                        gm.authToken
                                        song.trackToken
                                    )

                            Nothing ->
                                Cmd.none
                          )
                        ]
                    )

            RemovedFeedBack result ->
                ( model, gm, cmds [] )

            ToStations ->
                ( model, { gm | page = StationSelector }, cmds [] )

            ToChat ->
                ( model, { gm | page = ChatWindow }, cmds [] )

            ToPreviousSongs ->
                ( model
                , { gm
                    | page = PreviousSongs
                  }
                , cmds []
                )

            ToPlaying ->
                ( model, { gm | page = Player }, cmds [] )

            Logout ->
                ( model, { gm | page = LoginWindow }, cmds [ Port.logOutLocalStorage () ] )

            OpenDeleteStationPopup ->
                ( model, { gm | removingStationPopup = True }, cmds [] )

            OpenUpdateStationPopup ->
                ( model, { gm | updatingStationPopup = True }, cmds [] )

            HoveringAudio ->
                ( model, { gm | audioHover = True }, cmds [] )

            UnHoveringAudio ->
                ( model, { gm | audioHover = False }, cmds [] )

            SetAudio float ->
                ( model, { gm | audioLevel = Just float }, cmds [ Port.audioLevel float ] )

            Mute ->
                ( model, { gm | audioLevel = Just 0 }, cmds [ Port.audioLevel 0 ] )

            UnMute ->
                ( model, { gm | audioLevel = Just 1 }, cmds [ Port.audioLevel 1 ] )

            SetSeekLocation float ->
                ( model, { gm | seek = float }, cmds [ Port.getProgressBarWidth () ] )

            GotDetails result ->
                case result of
                    Ok color ->
                        ( model, { gm | dominantColor = color }, cmds [] )

                    Err error ->
                        ( model, { gm | dominantColor = "FFFFFF" }, cmds [] )



-- ΩΩΩ VIEW ΩΩΩ


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
viewProgressBar gm =
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
                            gm.currentTime
                            gm.songQueue
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


controller : GlobalModel -> List (Html Msg)
controller gm =
    let
        stationId =
            case gm.currentStation of
                Just station ->
                    station.id

                Nothing ->
                    ""

        ifEmptyQueue msg =
            if gm.songQueue /= [] then
                msg
            else
                NoOp

        currentRating =
            case List.head gm.songQueue of
                Just song ->
                    song.rating

                Nothing ->
                    0

        icon name msg styles =
            i
                [ class "material-icons controlIcons"
                , onClick msg
                , style styles
                ]
                [ text name ]
    in
        -- if clickable then
        [ icon "thumb_down_alt" (ifEmptyQueue (SendThumbsDown stationId)) [ ( "font-size", "20px" ) ]
        , icon "replay"
            (ifEmptyQueue ReplaySong)
            [ ( "font-size", "26px" )
            , ( "margin-left", "20px" )
            ]
        , icon
            (if gm.isPlaying then
                "pause"
             else
                "play_arrow"
            )
            (ifEmptyQueue TogglePause)
            [ ( "font-size", "40px" )
            , ( "margin-left", "10px" )
            , ( "margin-right", "10px" )
            ]
        , Icon.view "skip_next"
            [ Options.cs "material-icons controlIcons"
            , Options.css "font-size" "26px"
            , Options.css "margin-right" "20px"
            , Options.onClick (ifEmptyQueue (SkipSong stationId))
            , Tooltip.attach Mdl [ 690 ]
            ]
        , Tooltip.render Mdl
            [ 690 ]
            gm.mdl
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
                        (case (List.head (List.drop 1 gm.songQueue)) of
                            Just song ->
                                song.albumArt

                            Nothing ->
                                ""
                        )
                    ]
                    []
                , p [ style [ ( "text-align", "center" ), ( "margin-bottom", "0px" ) ] ]
                    [ text
                        (case (List.head (List.drop 1 gm.songQueue)) of
                            Just song ->
                                (song.songTitle ++ " - " ++ song.artistName)

                            Nothing ->
                                ""
                        )
                    ]
                ]
            ]
        , icon "thumb_up_alt"
            (if currentRating == 1 then
                RemoveThumbsUp stationId
             else
                SendThumbsUp stationId
            )
            [ ( "font-size", "20px" )
            , (if currentRating == 1 then
                ( "color", "#6499ef" )
               else
                ( "opacity", "1" )
              )
            ]
        ]


viewControls : GlobalModel -> Html Msg
viewControls gm =
    let
        stationId =
            case gm.currentStation of
                Just station ->
                    station.id

                Nothing ->
                    ""
    in
        div
            [ style
                [ ( "margin-top", "auto" )
                , ( "-webkit-user-select", "none" )
                , ( "height", "10%" )
                ]
            ]
            [ viewProgressBar gm
            , div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "90%" )
                    , ( "background-color", "#323842" )
                    , ( "display", "flex" )
                    , ( "justify-content", "center" )
                    , ( "align-items", "center" )
                    ]
                ]
                (controller gm)
            ]


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


audioSlider : GlobalModel -> Html Msg
audioSlider gm =
    Slider.view
        [ Slider.onChange SetAudio
        , Slider.max 1
        , Slider.min 0
        , Slider.step 0.1
        , Slider.value
            (case gm.audioLevel of
                Just level ->
                    level

                Nothing ->
                    0
            )
        , Options.css "width" "100px"
        , Options.css "padding-top" "8px"
        , Options.css "z-index" "2"
        , Options.css "margin-left" "0px"
        , Options.css "visibility"
            (if gm.audioHover then
                "visible"
             else
                "hidden"
            )
        ]


audioSelector : GlobalModel -> Html Msg
audioSelector gm =
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
                , ( "color", Types.textColor gm )
                ]
            , class "material-icons topIcons"
            , (if gm.audioLevel == Just 0 then
                onClick UnMute
               else
                onClick Mute
              )
            ]
            [ text
                (if gm.audioLevel == Just 0 then
                    "volume_off"
                 else if
                    (case gm.audioLevel of
                        Just level ->
                            level

                        Nothing ->
                            0
                    )
                        > 0.5
                 then
                    "volume_up"
                 else
                    "volume_down"
                )
            ]
        , audioSlider gm
        ]


viewTopMenu :
    GlobalModel
    -> Html Msg
viewTopMenu gm =
    div
        [ style
            [ ( "margin-left", "auto" )
            , ( "-webkit-user-select", "none" )
            ]
        ]
        [ let
            item icon name msg bool =
                Menu.item
                    [ Menu.onSelect msg
                    , if bool then
                        Menu.disabled
                      else
                        Options.cs "topMenuIcon"
                    ]
                    [ Icon.view icon
                        [ Options.css "width" "30px"
                        , Options.css "padding-right" "20px"
                        ]
                    , text name
                    ]

            noPrevious =
                gm.previousSongs == []

            emptyQueue =
                gm.songQueue == []
          in
            Menu.render Mdl
                [ 2 ]
                gm.mdl
                [ Menu.bottomRight
                , Menu.icon "keyboard_arrow_down"
                ]
                (case gm.page of
                    Player ->
                        [ item "apps" "Stations" ToStations False
                        , item "chat_bubble" "Chat" ToChat False
                        , item "first_page" "Previous Songs" ToPreviousSongs noPrevious
                        , item "clear" "Log out" Logout False
                        ]

                    StationSelector ->
                        [ item "play_circle_outline" "Player" ToPlaying False
                        , item "chat_bubble" "Chat" ToChat False
                        , item "first_page" "Previous Songs" ToPreviousSongs noPrevious
                        , item "clear" "Log out" Logout False
                        ]

                    ChatWindow ->
                        [ item "play_circle_outline" "Player" ToPlaying emptyQueue
                        , item "apps" "Stations" ToStations False
                        , item "first_page" "Previous Songs" ToPreviousSongs False
                        , item "clear" "Log out" Logout False
                        ]

                    PreviousSongs ->
                        [ item "play_circle_outline" "Player" ToPlaying emptyQueue
                        , item "apps" "Stations" ToStations False
                        , item "chat_bubble" "Chat" ToChat False
                        , item "clear" "Log out" Logout False
                        ]

                    LoginWindow ->
                        []
                )
        ]


viewTopBar :
    GlobalModel
    -> Html Msg
viewTopBar gm =
    let
        currentStationName =
            case gm.currentStation of
                Just station ->
                    station.name

                Nothing ->
                    ""

        icon csName msg styles name =
            i
                [ class csName
                , onClick msg
                , style
                    (List.append styles
                        (case currentStationName of
                            "" ->
                                [ ( "opacity", "0" )
                                , ( "pointer-events", "none" )
                                ]

                            _ ->
                                [ ( "pointer-events", "auto" ) ]
                        )
                    )
                ]
                [ text name ]
    in
        div
            [ style
                [ ( "position", "relative" )
                , ( "height", "10%" )
                , ( "-webkit-app-region", "drag" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "justify-content", "center" )
                    , ( "padding-top", "6px" )
                    ]
                ]
                [ icon "material-icons deleteStation"
                    OpenDeleteStationPopup
                    [ ( "margin-right", "1%" )
                    , ( "transition", "opacity 1s" )
                    , ( "color", Types.textColor gm )
                    ]
                    "delete"
                , p [ style [ ( "color", Types.textColor gm ) ] ] [ text currentStationName ]
                , icon "material-icons editStation"
                    OpenUpdateStationPopup
                    [ ( "margin-left", "1%" )
                    , ( "transition", "opacity 1s" )
                    , ( "color", Types.textColor gm )
                    ]
                    "edit"
                ]
            , div
                [ style
                    [ ( "display", "flex" )
                    ]
                ]
                [ audioSelector gm
                , viewTopMenu gm
                ]
            ]
