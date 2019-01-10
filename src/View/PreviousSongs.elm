module View.PreviousSongs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Util.Types as Types exposing (Song, GlobalModel)
import Material.Grid as Grid
import Material.Options as Options


view : List Song -> (Song -> msg) -> GlobalModel -> Html msg
view songs msg gm =
    Grid.grid
        [ Options.css "width" "100%"
        , Options.css "margin" "0px"
        , Options.css "overflow-y" "auto"
        ]
        (List.map
            (\song ->
                viewPreviousSong
                    song
                    msg
                    gm
            )
            songs
        )


viewPreviousSong : Song -> (Song -> msg) -> GlobalModel -> Grid.Cell msg
viewPreviousSong song msg gm =
    Grid.cell
        [ Grid.size Grid.Desktop 3
        , Options.css "margin-bottom" "25px "
        , Options.css "text-align" "center"
        ]
        [ div
            [ style
                [ ( "width", "150px" )
                , ( "height", "175px" )
                , ( "margin", "auto" )
                ]
            , onClick (msg song)
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
                    , ( "color", Types.textColor gm )
                    ]
                ]
                [ text song.songTitle ]
            ]
        ]
