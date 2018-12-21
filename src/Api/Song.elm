module Api.Song exposing (..)

import Util.Types as Types exposing (Song)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline
import Http exposing (Request)


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


getNext : String -> String -> Bool -> Request (List Song)
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
