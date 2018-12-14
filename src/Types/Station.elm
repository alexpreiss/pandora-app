module Types.Station exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Http exposing (Request)
import Dict exposing (Dict)


type alias Station =
    { id : String
    , name : String
    , art : String
    }


type alias SearchResult =
    { pandoraId : String
    , artistName : Maybe String
    , name : String
    , resultType : String
    , art : Maybe String
    , thorId : Maybe String
    , dominantColor : Maybe String
    }


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
        (Decode.field "name" Decode.string)
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


create : String -> String -> Request String
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
