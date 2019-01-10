module View.Player exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Util.Types as Types
    exposing
        ( GlobalModel
        )
import Material.Spinner as Spinner


viewSongInfo : GlobalModel -> Html msg
viewSongInfo gm =
    case List.head gm.songQueue of
        Just currentSong ->
            div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "align-items", "center" )
                    , ( "justify-content", "space-around" )
                    , ( "height", "100%" )
                    , ( "width", "100%" )
                    ]
                ]
                [ img
                    [ style
                        [ ( "-webkit-user-select", "none" )
                        , ( "height", "75%" )
                        , ( "box-shadow", "0.25px 0.25px black" )
                        , ( "border", "1px black solid" )
                        ]
                    , src currentSong.albumArt
                    ]
                    []
                , div
                    [ style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "justify-content", "space-evenly" )
                        , ( "height", "25%" )
                        , ( "width", "100%" )
                        ]
                    ]
                    [ p
                        [ style
                            [ ( "margin", "0px" )
                            , ( "text-align", "center" )
                            , ( "font-size", "3.5vmin" )
                            , ( "color", Types.textColor gm )
                            ]
                        ]
                        [ (case (List.head gm.songQueue) of
                            Just song ->
                                text song.songTitle

                            Nothing ->
                                if gm.currentStation == Nothing then
                                    text "Select a station to listen"
                                else
                                    Spinner.spinner [ Spinner.active True ]
                          )
                        ]
                    , p
                        [ style
                            [ ( "text-align", "center" )
                            , ( "font-size", "2.5vmin" )
                            , ( "margin", "0px" )
                            , ( "color", Types.textColor gm )
                            ]
                        ]
                        [ text
                            (case (List.head gm.songQueue) of
                                Just song ->
                                    song.artistName ++ " - " ++ song.albumTitle

                                Nothing ->
                                    ""
                            )
                        ]
                    ]
                ]

        Nothing ->
            div [] [ text "weird" ]
