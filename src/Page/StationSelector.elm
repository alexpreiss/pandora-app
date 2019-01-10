module Page.StationSelector exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Request)
import Material.Options as Options
import Material.Grid as Grid exposing (Device(..))
import Dict exposing (Dict)
import Api.Song as SongApi
import Api.Station as StationApi
import Util.Events as Events
import Util.Types as Types
    exposing
        ( Station
        , SearchResult
        , Song
        , GlobalModel
        , SelectorModel
        , SelectorState(..)
        , cmds
        )
import Task


-- ΩΩΩ MODEL ΩΩΩ


init : ( SelectorModel, Cmd Msg )
init =
    { state = Selecting
    , previousSongs = []
    , searchResults = Dict.empty
    , searchInput = ""
    }
        ! []



-- ΩΩΩ UPDATE ΩΩΩ


type Msg
    = StartStation String String String
    | StartedStation (Result Http.Error (List Song))
    | SearchForSong String
    | GotSearchedSongs (Result Http.Error (Dict String SearchResult))
    | CreateStation String
    | CreatedStation (Result Http.Error Station)
    | PlayPreviousSong Song
    | ToStations
    | ToSongSearch
    | NoOp


update : Msg -> SelectorModel -> GlobalModel -> ( SelectorModel, GlobalModel, Cmd Msg )
update msg model gm =
    case msg of
        StartStation id name art ->
            ( model
            , { gm
                | currentStation =
                    Just
                        { id = id
                        , name = name
                        , art = art
                        }
                , songQueue = []
                , page = Types.Player
              }
            , cmds
                [ Task.attempt StartedStation (SongApi.getNext id gm.authToken True)
                ]
            )

        StartedStation result ->
            case result of
                Ok songs ->
                    ( model
                    , { gm
                        | songQueue =
                            List.append
                                gm.songQueue
                                songs
                        , currentTime = 0
                        , isPlaying = True
                      }
                    , cmds []
                    )

                Err error ->
                    ( model, gm, cmds [] )

        ToStations ->
            ( model
            , { gm
                | page = Types.StationSelector
              }
            , cmds []
            )

        PlayPreviousSong song ->
            ( model
            , { gm
                | songQueue = song :: gm.songQueue
                , currentTime = 0
                , isPlaying = True
                , previousSongs = List.filter (\songTest -> not (songTest.trackToken == song.trackToken)) gm.previousSongs
                , page = Types.Player
              }
            , cmds []
            )

        ToSongSearch ->
            ( { model
                | state = Searching
                , searchResults = Dict.empty
                , searchInput = ""
              }
            , gm
            , cmds []
            )

        SearchForSong input ->
            if input == "" then
                ( { model
                    | searchResults = Dict.empty
                  }
                , gm
                , cmds []
                )
            else
                ( model, gm, cmds [ Http.send GotSearchedSongs (StationApi.search input gm.authToken) ] )

        GotSearchedSongs result ->
            case result of
                Ok songs ->
                    ( { model
                        | searchResults = songs
                      }
                    , gm
                    , cmds []
                    )

                Err error ->
                    let
                        log =
                            Debug.log "Error searching songs" error
                    in
                        ( model, gm, cmds [] )

        CreateStation musicToken ->
            ( { model | state = Types.Selecting }
            , gm
            , cmds
                [ Http.send CreatedStation
                    (StationApi.create
                        musicToken
                        gm.authToken
                    )
                ]
            )

        CreatedStation result ->
            case result of
                Ok station ->
                    ( model
                    , { gm
                        | stations = station :: gm.stations
                      }
                    , cmds []
                    )

                Err error ->
                    let
                        log =
                            Debug.log "Error creating station" error
                    in
                        ( model, gm, cmds [] )

        NoOp ->
            ( model, gm, cmds [] )



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : SelectorModel -> Sub Msg
subscriptions _ =
    Sub.none



-- ΩΩΩ VIEW ΩΩΩ


viewAddStation : GlobalModel -> Grid.Cell Msg
viewAddStation gm =
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
                    , ( "color", Types.textColor gm )
                    ]
                ]
                [ text "Add a station" ]
            ]
        ]


viewStation : Station -> GlobalModel -> Grid.Cell Msg
viewStation { id, name, art } gm =
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
            , onClick (StartStation id name art)
            ]
            [ img
                [ style
                    [ ( "height", "150px" )
                    , ( "width", "150px" )
                    , ( "border", "1px solid black" )
                    , ( "box-shadow", "0.25px 0.25px black" )
                    , ( "-webkit-user-select", "none" )
                    ]
                , src art
                ]
                []
            , p
                [ style
                    [ ( "text-align", "center" )
                    , ( "color", Types.textColor gm )
                    ]
                ]
                [ text name ]
            ]
        ]


viewStationSelector : GlobalModel -> Html Msg
viewStationSelector gm =
    Grid.grid
        [ Options.css "width" "100%"
        , Options.css "margin" "0px"
        , Options.css "overflow-y" "auto"
        , (Options.css) "background-color" "rgba(255, 255, 255, 0)"
        ]
        (viewAddStation gm
            :: (List.map
                    (\station -> viewStation station gm)
                    gm.stations
               )
        )


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
            , (case result.name of
                Just name ->
                    p [ style [ ( "margin", "0" ) ] ] [ text name ]

                Nothing ->
                    text ""
              )
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


viewSearchResults : Dict String SearchResult -> Html Msg
viewSearchResults searchResults =
    if searchResults == Dict.empty then
        h4 [] [ text "No results" ]
    else
        div
            [ style
                [ ( "overflow-y", "auto" )
                , ( "width", "100%" )
                ]
            ]
            (List.map viewSearchResult (Dict.values searchResults))


viewSearch : SelectorModel -> Html Msg
viewSearch model =
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
                , ( "flex-shrink", "0" )
                , ( "margin-bottom", "5%" )
                ]
            , onInput SearchForSong
            , Events.blockSpacebar NoOp
            ]
            []
        , viewSearchResults model.searchResults
        ]


view : GlobalModel -> Html Msg
view gm =
    let
        model =
            gm.selectorModel
    in
        case model.state of
            Selecting ->
                viewStationSelector gm

            Searching ->
                viewSearch model
