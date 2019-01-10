module OldMain exposing (..)

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
import Util.Types as Types exposing (Song, Station, GlobalModel, SearchResult, Page)
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


init : ( GlobalModel, Cmd Msg )
init



-- currentSong : GlobalModel -> Song
-- currentSong model =
--     case model.state of
--         LoggingIn fields ->
--             { songTitle = ""
--             , trackLength = 0
--             , rating = 0
--             , audioURL = ""
--             , artistName = ""
--             , albumTitle = ""
--             , albumArt = ""
--             , trackToken = ""
--             }
--
--         Playing fields ->
--             case (List.head fields.songQueue) of
--                 Just song ->
--                     song
--
--                 Nothing ->
--                     { songTitle = ""
--                     , trackLength = 0
--                     , rating = 0
--                     , audioURL = ""
--                     , artistName = ""
--                     , albumTitle = ""
--                     , albumArt = ""
--                     , trackToken = ""
--                     }
-- type alias AddingStationFields =
--     { searchResults : Dict String SearchResult
--     , searchInput : String
--     }
--
--
-- type alias PlayingFields =
--     { authToken : String
--     , currentStation : Maybe Station
--     , songQueue : List Song
--     , currentTime : Float
--     , audioLevel : Float
--     , audioHover : Bool
--     , isPlaying : Bool
--     , playingState : PlayingState
--     , seek : Float
--     , email : String
--     , username : String
--     , newUser : Bool
--     , removingStationPopup : Bool
--     , updatingStationPopup : Bool
--     , updateStationNameInput : String
--     }

--
--
--
--
-- -- ΩΩΩ UPDATE ΩΩΩ
--
--


type Msg
    = Logout
      -- Controls
      --
      -- | SetCurrentTime Float
      -- | SetSeekLocation Float
      -- | SetNewTime Float
      -- | SongEnded String
      --
      -- | LoadNextSongs String
      --
      --   --  Audio
      -- | SetAudio Float
      -- | Mute
      -- | UnMute
      -- | HoveringAudio
      -- | UnHoveringAudio
      -- Navigation
    | ToPlaying
    | ToChat
      -- Feedback
      -- | SendThumbsDown String
      -- | SendThumbsUp String
      --
      --   -- Misc
    | Mdl (Material.Msg Msg)



-- | NoOp
-- | NoOp1 String
-- | KeyDown Int
--   -- Removing Stations
-- | OpenRemoveStationPopup
-- | CloseRemoveStationPopup
-- | RemoveStation Station
-- | RemovedStation (Result Http.Error String)
--   -- Updating Stations
-- | UpdateStation
-- | UpdatedStation (Result Http.Error String)
-- | OpenUpdateStationPopup
-- | CloseUpdateStationPopup
-- | NameInputToModel String


update : Msg -> GlobalModel -> ( GlobalModel, Cmd Msg )
update msg model =
    case msg of
        -- LoadNextSongs id ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             model
        --                 ! [ Http.send LoadedNextSongs
        --                         (Fragment.getNext id fields.authToken False)
        --                   ]
        --
        -- LoadedNextSongs result ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             case result of
        --                 Ok songs ->
        --                     { model
        --                         | state =
        --                             Playing
        --                                 { fields
        --                                     | songQueue =
        --                                         List.append
        --                                             fields.songQueue
        --                                             songs
        --                                 }
        --                     }
        --                         ! []
        --
        --                 Err error ->
        --                     { model | loadingSongError = True } ! []
        --
        -- SongEnded id ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing
        --                         { fields
        --                             | songQueue = (List.drop 1 fields.songQueue)
        --                             , previousSongs =
        --                                 currentSong model :: fields.previousSongs
        --                             , currentTime = 0
        --                             , isPlaying = True
        --                         }
        --             }
        --                 ! [ if List.length fields.songQueue == 2 then
        --                         Http.send LoadedNextSongs
        --                             (Fragment.getNext id fields.authToken False)
        --                     else
        --                         Cmd.none
        --                   ]
        --
        --
        --
        -- SetCurrentTime _ ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | currentTime = fields.currentTime + 1 }
        --             }
        --                 ! []
        --
        --
        --
        --
        --
        --
        --

        --
        --
        --
        --
        --
        -- SetAudio level ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | audioLevel = level }
        --             }
        --                 ! [ audioLevel level ]
        --
        -- HoveringAudio ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | audioHover = True }
        --             }
        --                 ! []
        --
        -- UnHoveringAudio ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | audioHover = False }
        --             }
        --                 ! []
        Mdl msg ->
            Material.update Mdl msg model

        -- NoOp ->
        --     model ! []
        ToPlaying ->
            { model
                | page = Player

                -- , getStationError = False
                -- , loadingSongError = False
                -- , startStationError = False
            }
                ! []

        --
        -- Mute ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | audioLevel = 0 }
        --             }
        --                 ! [ audioLevel 0 ]
        --
        -- UnMute ->
        --     case model.state of
        --         LoggingIn fields ->
        --             model ! []
        --
        --         Playing fields ->
        --             { model
        --                 | state =
        --                     Playing { fields | audioLevel = 1 }
        --             }
        --                 ! [ audioLevel 1 ]
        Logout ->
            { model
                | page = Login
            }
                ! []

        --
        -- RememberMe val ->
        --     case model.state of
        --         LoggingIn record ->
        --             { model
        --                 | state =
        --                     LoggingIn { record | remember = val }
        --             }
        --                 ! []
        --
        --         Playing record ->
        --             model ! []
        --
        -- SetSeekLocation coord ->
        --     case model.state of
        --         LoggingIn record ->
        --             model ! []
        --
        --         Playing record ->
        --             { model
        --                 | state =
        --                     Playing { record | seek = coord }
        --             }
        --                 ! [ getProgressBarWidth () ]
        --
        -- SetNewTime totalWidth ->
        --     case model.state of
        --         LoggingIn record ->
        --             model ! []
        --
        --         Playing record ->
        --             let
        --                 newTime =
        --                     (record.seek
        --                         / totalWidth
        --                     )
        --                         * (toFloat (currentSong model).trackLength)
        --             in
        --                 { model
        --                     | state =
        --                         Playing { record | currentTime = newTime }
        --                 }
        --                     ! [ sendNewTime newTime ]
        ToChat ->
            { model | page = Chat } ! []



-- if Debug.log "bool" record.newUser == False then
--     { model
--         | state =
--             Playing
--                 { record
--                     | playingState =
--                         Chatting
--                             { email = record.email
--                             , chatInput = ""
--                             , chats = []
--                             , username = record.username
--                             , newUser = False
--                             , userNameInput = ""
--                             }
--                 }
--     }
--         ! [ rememberUsername record.username
--           , newUser ()
--           ]
-- else
--     { model
--         | state =
--             Playing
--                 { record
--                     | playingState =
--                         Chatting
--                             { email = record.email
--                             , chatInput = ""
--                             , chats = []
--                             , username = ""
--                             , newUser = True
--                             , userNameInput = ""
--                             }
--                 }
--     }
--         ! [ Http.send GotChats (Chat.getAll record.authToken)
--           ]
-- KeyDown code ->
--     case model.state of
--         LoggingIn record ->
--             model
--                 ! if code == 13 then
--                     (if record.remember then
--                         [ Http.send LoggedInRemember
--                             (login
--                                 { email = record.email
--                                 , password = record.password
--                                 }
--                             )
--                         , rememberEmail record.email
--                         , rememberPassword record.password
--                         ]
--                      else
--                         [ Http.send LoggedIn
--                             (login
--                                 { email = record.email
--                                 , password = record.password
--                                 }
--                             )
--                         ]
--                     )
--                   else
--                     [ Cmd.none ]
--
--         Playing record ->
--             (if code == 32 then
--                 { model
--                     | keyPress = code
--                     , state =
--                         Playing
--                             { record | isPlaying = not record.isPlaying }
--                 }
--              else
--                 { model
--                     | state =
--                         Playing
--                             { record | isPlaying = record.isPlaying }
--                 }
--             )
--                 ! [ (if code == 32 then
--                         togglePause ()
--                      else
--                         Cmd.none
--                     )
--                   ]
--
-- NoOp1 _ ->
--     model ! []
--
-- RemoveStation removedStation ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model
--                 | state =
--                     Playing
--                         { record
--                             | stations =
--                                 (List.filter
--                                     (\station -> station.id /= removedStation.id)
--                                     record.stations
--                                 )
--                         }
--             }
--                 ! [ Http.send RemovedStation (Station.remove removedStation.id record.authToken) ]
--
-- RemovedStation _ ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model
--                 | state =
--                     Playing
--                         { record
--                             | playingState = SelectingStation
--                             , removingStationPopup = False
--                             , isPlaying = False
--                             , currentTime = 0
--                             , currentStation = Nothing
--                         }
--             }
--                 ! []
--
-- OpenRemoveStationPopup ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model | state = Playing { record | removingStationPopup = True } } ! []
--
-- CloseRemoveStationPopup ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model | state = Playing { record | removingStationPopup = False } } ! []
--
-- OpenUpdateStationPopup ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model | state = Playing { record | updatingStationPopup = True } } ! []
--
-- CloseUpdateStationPopup ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model | state = Playing { record | updatingStationPopup = False } } ! []
--
-- UpdateStation ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             let
--                 stationId =
--                     case record.currentStation of
--                         Just station ->
--                             station.id
--
--                         Nothing ->
--                             ""
--             in
--                 model ! [ Http.send UpdatedStation (Station.update stationId record.updateStationNameInput record.authToken) ]
--
-- UpdatedStation result ->
--     case result of
--         Ok result ->
--             case model.state of
--                 LoggingIn record ->
--                     model ! []
--
--                 Playing record ->
--                     let
--                         station =
--                             case record.currentStation of
--                                 Just station ->
--                                     station
--
--                                 Nothing ->
--                                     { id = ""
--                                     , name = ""
--                                     , art = ""
--                                     }
--                     in
--                         { model
--                             | state =
--                                 Playing
--                                     { record
--                                         | currentStation =
--                                             Just
--                                                 { id = station.id
--                                                 , name = record.updateStationNameInput
--                                                 , art = station.art
--                                                 }
--                                         , updatingStationPopup = False
--                                     }
--                         }
--                             ! []
--
--         Err error ->
--             let
--                 log =
--                     Debug.log "Error updating station" error
--             in
--                 model ! []
--
-- NameInputToModel input ->
--     case model.state of
--         LoggingIn record ->
--             model ! []
--
--         Playing record ->
--             { model | state = Playing { record | updateStationNameInput = input } } ! []
-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : GlobalModel -> Sub Msg
subscriptions gm =
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
                    record.removingStationPopup
                    record.updatingStationPopup
                ]

        LoggingIn record ->
            viewLogin model record.remember record.failed
