module Types.Feedback exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Http exposing (Request)


remove : String -> String -> Request String
remove authToken songId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/station/deleteFeedback"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isPositive", (Encode.bool False) )
                    , ( "trackToken", (Encode.string songId) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }


send : String -> String -> Bool -> Request String
send authToken songId isPositive =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            ]
        , url = "http://localhost:8080/api?url=https://www.pandora.com/api/v1/station/addFeedback"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "isPositive", (Encode.bool isPositive) )
                    , ( "trackToken", (Encode.string songId) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }
