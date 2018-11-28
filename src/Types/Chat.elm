module Types.Chat exposing (..)

import Json.Decode as Decode
import Http exposing (Request)


type alias Chat =
    { id : Int
    , email : String
    , content : String
    , username : String
    }


chatDecoder : Decode.Decoder Chat
chatDecoder =
    Decode.map4 Chat
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "content" Decode.string)
        (Decode.field "username" Decode.string)


getAll :
    String
    -> Request (List Chat)
getAll authToken =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "X-CsrfToken" "coolestToken"
            , Http.header "X-AuthToken" authToken
            , Http.header "Content-Type" "application/json"
            ]
        , body = Http.emptyBody
        , url = "http://localhost:8000/getchats"
        , expect = Http.expectJson (Decode.list chatDecoder)
        , timeout = Nothing
        , withCredentials = False
        }
