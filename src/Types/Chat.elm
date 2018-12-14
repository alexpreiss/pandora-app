module Types.Chat exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (Request)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material
import Material.Options as Options
import Material.Button as Button
import Material.Icon as Icon


-- Types


type alias Chat =
    { id : Int
    , email : String
    , username : String
    , content : String
    }



-- Decoder Functions


chatDecoder : Decode.Decoder Chat
chatDecoder =
    Decode.map4 Chat
        (Decode.field "id" Decode.int)
        (Decode.field "email" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "content" Decode.string)



-- Request Functions


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



-- View Functions


viewChat : Chat -> Html msg
viewChat chat =
    div
        [ style
            [ ( "display", "flex" )
            , ( "margin-left", "1%" )
            ]
        ]
        [ if chat.username /= "" then
            p
                [ style
                    [ ( "font-weight", "bold" )
                    ]
                ]
                [ text chat.username ]
          else
            p
                [ style
                    [ ( "font-weight", "bold" )
                    ]
                ]
                [ text chat.email ]
        , p
            [ style
                [ ( "margin-left", "1.5%" )
                , ( "margin-top", "0.04%" )
                ]
            ]
            [ text chat.content ]
        ]


allChats : List Chat -> List (Html msg)
allChats chatList =
    List.map viewChat chatList


chatRooms : Html msg
chatRooms =
    div
        [ style
            [ ( "background-color", "rgb(223, 223, 223)" )
            , ( "width", "15%" )
            , ( "height", "95%" )
            ]
        ]
        []


darkEle : Html msg
darkEle =
    div
        [ style
            [ ( "z-index", "2" )
            , ( "position", "absolute" )
            , ( "background-color", "rgba(0, 0, 0, 0.5)" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            , ( "right", "0" )
            , ( "left", "0" )
            ]
        ]
        []


chatInput : (String -> msg) -> String -> msg -> (Material.Msg msg -> msg) -> Material.Model -> msg -> Html msg
chatInput inputMsg chatInputVal noOp mdlMsg modelMdl sendMsg =
    div
        [ style
            [ ( "height", "6%" )
            , ( "width", "89.75%" )
            , ( "margin-top", "1%" )
            , ( "margin-bottom", "3%" )
            , ( "display", "flex" )
            ]
        ]
        [ input
            [ style
                [ ( "padding-left", "5px" )
                , ( "height", "100%" )
                , ( "width", "95%" )
                ]
            , value chatInputVal
            , onInput inputMsg
            , Html.Events.onWithOptions "keydown"
                { stopPropagation = True
                , preventDefault = False
                }
                (Decode.succeed noOp)
            ]
            []
        , (Button.render mdlMsg
            [ 7834 ]
            modelMdl
            [ Button.ripple
            , Options.css "height" "100%"
            , Options.css "margin-top" ".45%"
            , Options.onClick sendMsg
            ]
            [ text "Send" ]
          )
        ]


chatLogin :
    String
    -> (String -> msg)
    -> Material.Model
    -> (Material.Msg msg -> msg)
    -> msg
    -> msg
    -> Html msg
chatLogin nameInput userNameInputMsg modelMdl mdlMsg noOp setUserNameMsg =
    div
        [ style
            [ ( "z-index", "2" )
            , ( "position", "absolute" )
            , ( "background-color", "white" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "height", "60%" )
            , ( "width", "50%" )
            ]
        ]
        [ h3 [ style [ ( "text-align", "center" ) ] ]
            [ text "Please enter a username: " ]
        , p [ style [ ( "text-align", "center" ) ] ]
            [ text "This is what other users will see when you send a message" ]
        , p
            [ style
                [ ( "text-align", "center" )
                , ( "margin-top", "10%" )
                ]
            , class "chatLoginAltOpt"
            ]
            [ text "Or click here to use your email instead" ]
        , input
            [ type_ "text"
            , onInput userNameInputMsg
            , style
                [ ( "height", "12%" )
                , ( "padding-left", "2%" )
                , ( "font-size", "100%" )
                , ( "margin-top", "auto" )
                ]
            ]
            []
        , (Button.render mdlMsg
            [ 18 ]
            modelMdl
            [ Button.minifab
            , Button.ripple
            , (if nameInput == "" then
                Options.onClick noOp
               else
                Options.onClick setUserNameMsg
              )
            , Options.css "height" "20%"
            , Options.css "margin-bottom" "0px"
            ]
            [ Icon.i "arrow_right_alt" ]
          )
        ]


viewWindow :
    { email : String
    , chatInput : String
    , chats : List Chat
    , username : String
    , newUser : Bool
    , userNameInput : String
    }
    -> (String -> msg)
    -> (Material.Msg msg -> msg)
    -> Material.Model
    -> msg
    -> (String -> msg)
    -> msg
    -> msg
    -> Html msg
viewWindow fields inputMsg mdlMsg modelMdl sendMsg userNameInputMsg noOp setUserNameMsg =
    div
        [ style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "height", "100%" )
                , ( "width", "100%" )
                ]
            ]
            [ chatRooms

            -- CHAT SELECTOR ^^^
            , div
                [ style
                    [ ( "margin-top", "0px" )
                    , ( "width", "84%" )
                    , ( "height", "95%" )
                    , ( "overflow-y", "auto" )
                    ]
                ]
                (allChats fields.chats)
            ]

        -- CHAT ^^^
        , chatInput inputMsg fields.chatInput noOp mdlMsg modelMdl sendMsg
        , if fields.newUser then
            darkEle
          else
            text ""
        , if fields.newUser then
            chatLogin fields.userNameInput userNameInputMsg modelMdl mdlMsg noOp setUserNameMsg
          else
            text ""
        ]
