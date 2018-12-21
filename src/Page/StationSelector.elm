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
        )


-- ΩΩΩ MODEL ΩΩΩ


type State
    = Selecting
    | Searching
    | PreviousSongs


type alias Model =
    { state : State
    , stations : List Station
    , previousSongs : List Song
    , searchResults : Dict String SearchResult
    , searchInput : String
    }


init : ( Model, Cmd Msg )
init =
    { state = Selecting
    , stations = []
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
    | ToPreviousSongs
    | NoOp


cmds : List (Cmd Msg) -> Cmd Msg
cmds cmdList =
    Cmd.batch cmdList


update : Msg -> Model -> GlobalModel -> ( Model, GlobalModel, Cmd Msg )
update msg model globalModel =
    case msg of
        StartStation id name art ->
            ( model
            , { globalModel
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
                [ Http.send StartedStation
                    (SongApi.getNext id globalModel.authToken True)
                ]
            )

        StartedStation result ->
            case result of
                Ok songs ->
                    ( model
                    , { globalModel
                        | songQueue =
                            List.append
                                globalModel.songQueue
                                songs
                        , currentTime = 0
                        , isPlaying = True
                      }
                    , cmds []
                    )

                Err error ->
                    ( model, globalModel, cmds [] )

        ToStations ->
            ( model
            , { globalModel
                | page = Types.StationSelector
              }
            , cmds []
            )

        ToPreviousSongs ->
            ( { model | state = PreviousSongs }, globalModel, cmds [] )

        PlayPreviousSong song ->
            ( model
            , { globalModel
                | songQueue = song :: globalModel.songQueue
                , currentTime = 0
                , isPlaying = True
                , previousSongs = List.filter (\songTest -> not (songTest.trackToken == song.trackToken)) globalModel.previousSongs
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
            , globalModel
            , cmds []
            )

        SearchForSong input ->
            if input == "" then
                ( { model
                    | searchResults = Dict.empty
                  }
                , globalModel
                , cmds []
                )
            else
                ( model, globalModel, cmds [ Http.send GotSearchedSongs (StationApi.search input globalModel.authToken) ] )

        GotSearchedSongs result ->
            case Debug.log "songs" result of
                Ok songs ->
                    ( { model
                        | searchResults = songs
                      }
                    , globalModel
                    , cmds []
                    )

                Err error ->
                    let
                        log =
                            Debug.log "Error searching songs" error
                    in
                        ( model, globalModel, cmds [] )

        CreateStation musicToken ->
            ( model
            , { globalModel | page = Types.StationSelector }
            , cmds
                [ Http.send CreatedStation
                    (StationApi.create
                        musicToken
                        globalModel.authToken
                    )
                ]
            )

        CreatedStation result ->
            case result of
                Ok station ->
                    ( model
                    , { globalModel
                        | stations = station :: model.stations
                      }
                    , cmds []
                    )

                Err error ->
                    let
                        log =
                            Debug.log "Error creating station" error
                    in
                        ( model, globalModel, cmds [] )

        NoOp ->
            ( model, globalModel, cmds [] )



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ΩΩΩ VIEW ΩΩΩ


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


viewPreviousSongs : List Song -> Html Msg
viewPreviousSongs previousSongs =
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
            previousSongs
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


viewStation : Station -> Grid.Cell Msg
viewStation { id, name, art } =
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
                    ]
                ]
                [ text name ]
            ]
        ]


viewStationSelector : GlobalModel -> Html Msg
viewStationSelector globalModel =
    Grid.grid
        [ Options.css "width" "100%"
        , Options.css "margin" "0px"
        , Options.css "overflow-y" "auto"
        ]
        (viewAddStation
            :: (List.map
                    (\station -> viewStation station)
                    globalModel.stations
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


viewSearch : Model -> Html Msg
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


view : Model -> GlobalModel -> Html Msg
view model globalModel =
    case model.state of
        Selecting ->
            viewStationSelector globalModel

        Searching ->
            viewSearch model

        PreviousSongs ->
            viewPreviousSongs model.previousSongs
