module Api.Song exposing (..)

import Util.Types as Types exposing (Song)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline
import Http exposing (Request)
import Task exposing (Task)


fragmentListDecoder : Decode.Decoder (List Song)
fragmentListDecoder =
    (Decode.at [ "tracks" ]
        (Decode.list
            fragmentDecoder
        )
    )


fragmentDecoder : Decode.Decoder Song
fragmentDecoder =
    Pipeline.decode Song
        |> Pipeline.required "songTitle" Decode.string
        |> Pipeline.required "trackLength" Decode.int
        |> Pipeline.required "rating" Decode.int
        |> Pipeline.required "audioURL" Decode.string
        |> Pipeline.required "artistName" Decode.string
        |> Pipeline.required "albumTitle" Decode.string
        |> Pipeline.requiredAt [ "albumArt", "4", "url" ] Decode.string
        |> Pipeline.required "trackToken" Decode.string
        |> Pipeline.required "pandoraId" Decode.string
        |> Pipeline.hardcoded "FFFFFF"


getNext : String -> String -> Bool -> Task Http.Error (List Song)
getNext stationId authToken isStationStart =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "https://www.pandora.com/api/v1/playlist/getFragment"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "stationId", (Encode.string stationId) )
                    , ( "isStationStart", (Encode.bool isStationStart) )
                    , ( "fragmentRequestReason", (Encode.string "Normal") )
                    , ( "audioFormat", (Encode.string "mp3-hifi") )
                    , ( "startingAtTrackId", Encode.null )
                    , ( "onDemandArtistMessageArtistUidHex", Encode.null )
                    , ( "onDemandArtistMessageIdHex", Encode.null )
                    ]
                )
        , expect = Http.expectJson fragmentListDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.andThen
            (\songs ->
                List.map (getDetails authToken >> Http.toTask) songs
                    |> Task.sequence
            )


detailDecoder : Song -> Decode.Decoder Song
detailDecoder song =
    (Decode.at [ "annotations", song.pandoraId, "icon", "dominantColor" ]
        (Decode.oneOf
            [ Decode.string
            , Decode.succeed "FFFFFF"
            ]
        )
    )
        |> Decode.map (\domColor -> { song | dominantColor = domColor })


getDetails : String -> Song -> Request Song
getDetails authToken song =
    let
        pandoraId =
            song.pandoraId
    in
        Http.request
            { method = "POST"
            , headers =
                [ Http.header "X-CsrfToken" "coolestToken"
                , Http.header "X-AuthToken" authToken
                ]
            , url = "https://www.pandora.com/api/v4/catalog/getDetails"
            , body =
                Http.jsonBody
                    (Encode.object
                        [ ( "pandoraId", (Encode.string pandoraId) )
                        ]
                    )
            , expect =
                Http.expectJson (detailDecoder song)
            , timeout = Nothing
            , withCredentials = False
            }
