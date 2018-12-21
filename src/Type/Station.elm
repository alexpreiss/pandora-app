module Type.Station exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Material
import Material.Options as Options
import Material.Button as Button
import Util.Events as Events


-- View Functions


updatingPopup :
    Material.Model
    -> (Material.Msg msg -> msg)
    -> Station
    -> msg
    -> msg
    -> msg
    -> (String -> msg)
    -> Html msg
updatingPopup modelMdl mdlMsg station closePopupMsg updateMsg noOp inputMsg =
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
            , onInput inputMsg
            , Events.blockSpacebar noOp
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
            [ Button.render mdlMsg
                [ 987 ]
                modelMdl
                [ Options.onClick closePopupMsg
                , Button.raised
                , Button.colored
                , Button.ripple
                , Options.css "height" "75px"
                , Options.css "width" "125px"
                ]
                [ text "Cancel" ]
            , Button.render mdlMsg
                [ 573 ]
                modelMdl
                [ Options.onClick updateMsg
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
    Material.Model
    -> (Material.Msg msg -> msg)
    -> Station
    -> msg
    -> (Station -> msg)
    -> Html msg
removingPopup modelMdl mdlMsg station closePopupMsg removeMsg =
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
            [ Button.render mdlMsg
                [ 987 ]
                modelMdl
                [ Options.onClick closePopupMsg
                , Button.raised
                , Button.colored
                , Button.ripple
                , Options.css "height" "75px"
                , Options.css "width" "125px"
                ]
                [ text "Cancel" ]
            , Button.render mdlMsg
                [ 573 ]
                modelMdl
                [ Options.onClick (removeMsg station)
                , Button.raised
                , Button.colored
                , Button.ripple
                , Options.css "height" "75px"
                , Options.css "width" "125px"
                ]
                [ text "Delete" ]
            ]
        ]
