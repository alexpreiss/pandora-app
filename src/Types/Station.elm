module Types.Station exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Http exposing (Request)


type alias Station =
    { id : String
    , name : String
    , art : String
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
                    "img/illfigurethisout"
                )
              )
            , (Decode.succeed "img/illfigurethisout")
            ]
        )


stationListDecoder : Decode.Decoder (List Station)
stationListDecoder =
    (Decode.at [ "stations" ]
        (Decode.list
            stationDecoder
        )
    )


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
