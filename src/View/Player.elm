module View.Player exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Util.Types exposing (GlobalModel)
import Material.Spinner as Spinner


viewSongInfo : GlobalModel -> Html msg
viewSongInfo gm =
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
                , ( "border", "1px black solid" )
                ]
            , src
                (case (List.head gm.songQueue) of
                    Just song ->
                        song.albumArt

                    Nothing ->
                        "fillerIMG.jpg"
                )
            , class "playerAlbumArt"
            ]
            []
        , p
            [ style
                [ ( "margin-top", "4%" )
                , ( "margin-bottom", "0px" )
                ]
            , class "songTitle"
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
                [ ( "margin-top", "5px" )
                ]
            , class "songDetails"
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
