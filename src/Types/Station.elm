module Types.Station exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Http exposing (Request)
import Dict exposing (Dict)
import Material
import Material.Options as Options
import Material.Button as Button
import Util.Events as Events


-- Types


type alias Station =
    { id : String
    , name : String
    , art : String
    }


type alias SearchResult =
    { pandoraId : String
    , artistName : Maybe String
    , name : Maybe String
    , resultType : String
    , art : Maybe String
    , thorId : Maybe String
    , dominantColor : Maybe String
    }



-- Decoder Functions


createdStationDecoder : Decode.Decoder Station
createdStationDecoder =
    Decode.map3 Station
        (Decode.field "stationId" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.oneOf
            [ (Decode.field "art"
                (decodeListLast
                    (Decode.field "url" Decode.string)
                    "fillerImg.jpg"
                )
              )
            , (Decode.succeed "fillerIMG.jpg")
            ]
        )


decodeListLast : Decode.Decoder a -> a -> Decode.Decoder a
decodeListLast decoder default =
    Decode.list (Decode.succeed ())
        |> Decode.andThen
            (\list ->
                if List.length list == 0 then
                    Decode.succeed default
                else
                    Decode.index (List.length list - 1) decoder
            )


stationDecoder : Decode.Decoder Station
stationDecoder =
    Decode.map3 Station
        (Decode.field "stationId" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.oneOf
            [ (Decode.field "art"
                (decodeListLast
                    (Decode.field "url" Decode.string)
                    "fillerImg.jpg"
                )
              )
            , (Decode.succeed "fillerIMG.jpg")
            ]
        )


searchResultListDecoder : Decode.Decoder (Dict String SearchResult)
searchResultListDecoder =
    Decode.field "annotations" (Decode.dict searchResultDecoder)


searchResultDecoder : Decode.Decoder SearchResult
searchResultDecoder =
    Decode.map7 SearchResult
        (Decode.field "pandoraId" Decode.string)
        (Decode.maybe (Decode.field "artistName" Decode.string))
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.field "type" Decode.string)
        (Decode.maybe
            (Decode.oneOf
                [ (Decode.at
                    [ "icon", "artId" ]
                    Decode.string
                  )
                , (Decode.at
                    [ "icon", "artUrl" ]
                    Decode.string
                  )
                ]
            )
        )
        (Decode.maybe (Decode.at [ "icon", "thorId" ] Decode.string))
        (Decode.maybe
            (Decode.at
                [ "icon", "dominantColor" ]
                Decode.string
            )
        )


stationListDecoder : Decode.Decoder (List Station)
stationListDecoder =
    (Decode.at [ "stations" ]
        (Decode.list
            stationDecoder
        )
    )



-- Request Functions


remove : String -> String -> Request String
remove stationId authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url =
            "https://www.pandora.com/api/v1/station/removeStation"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "stationId", (Encode.string stationId) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }


search : String -> String -> Request (Dict String SearchResult)
search searchText authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url =
            "https://www.pandora.com/api/v3/sod/search"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "annotate", (Encode.bool True) )
                    , ( "count", (Encode.int 20) )
                    , ( "query", (Encode.string searchText) )
                    , ( "types"
                      , (Encode.list
                            [ (Encode.string "AL")
                            , (Encode.string "AR")
                            , (Encode.string "TR")
                            , (Encode.string "ST")
                            ]
                        )
                      )
                    ]
                )
        , expect = Http.expectJson searchResultListDecoder
        , timeout = Nothing
        , withCredentials = False
        }


create : String -> String -> Request Station
create musicToken authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "https://www.pandora.com/api/v1/station/createStation"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "pandoraId", (Encode.string musicToken) )
                    ]
                )
        , expect = Http.expectJson createdStationDecoder
        , timeout = Nothing
        , withCredentials = False
        }


update : String -> String -> String -> Request String
update stationId input authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "https://www.pandora.com/api/v1/station/updateStation"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "name", (Encode.string input) )
                    , ( "stationId", (Encode.string stationId) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }


get : String -> Request (List Station)
get authToken =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "https://www.pandora.com/api/v1/station/getStations"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "pageSize", (Encode.int 300) )
                    ]
                )
        , expect = Http.expectJson stationListDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- View Functions


viewSearchResult : (String -> msg) -> SearchResult -> Html msg
viewSearchResult createMsg result =
    div
        [ style
            [ ( "display", "flex" )
            , ( "width", "100%" )
            , ( "border-top", "0.5px black solid" )
            ]
        , onClick (createMsg result.pandoraId)
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


viewSearchResults : Dict String SearchResult -> (String -> msg) -> Html msg
viewSearchResults searchResults createMsg =
    if searchResults == Dict.empty then
        h4 [] [ text "No results" ]
    else
        div
            [ style
                [ ( "overflow-y", "auto" )
                , ( "width", "100%" )
                ]
            ]
            (List.map (viewSearchResult createMsg) (Dict.values searchResults))


viewSearch : Dict String SearchResult -> (String -> msg) -> msg -> (String -> msg) -> Html msg
viewSearch searchResults searchMsg noOp createMsg =
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
            , onInput searchMsg
            , Events.blockSpacebar noOp
            ]
            []
        , viewSearchResults searchResults createMsg
        ]


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
        , h5 [ style [ ( "text-align", "center" ) ] ] [ text ("\"" ++ station.name ++ "\"?") ]
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
