module Api.Login exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (Request)


authDecode : Decode.Decoder String
authDecode =
    (Decode.at [ "authToken" ] Decode.string)


login :
    { email : String
    , password : String
    }
    -> Request String
login info =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            ]
        , url = "https://www.pandora.com/api/v1/auth/login"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "username", (Encode.string info.email) )
                    , ( "password", (Encode.string info.password) )
                    , ( "existingAuthToken", Encode.null )
                    , ( "keepLoggedIn", (Encode.bool True) )
                    ]
                )
        , expect = Http.expectJson authDecode
        , timeout = Nothing
        , withCredentials = False
        }
