module Api.Chat exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Util.Types exposing (Chat)
import Http exposing (Request)


chatDecoder : Decode.Decoder Chat
chatDecoder =
    Decode.map4 Chat
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "content" Decode.string)


getAll :
    String
    -> Request (List Chat)
getAll authToken =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , body = Http.emptyBody
        , url = "https://pandora-app-alexpreiss.herokuapp.com/getchats"
        , expect = Http.expectJson (Decode.list chatDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


send :
    { email : String
    , username : String
    , content : String
    }
    -> Request String
send items =
    Http.request
        { method = "POST"
        , headers =
            []
        , url = "https://pandora-app-alexpreiss.herokuapp.com/sendchat"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", (Encode.string items.email) )
                    , ( "username", (Encode.string items.username) )
                    , ( "content", (Encode.string items.content) )
                    ]
                )
        , expect = Http.expectJson (Decode.succeed "")
        , timeout = Nothing
        , withCredentials = False
        }
