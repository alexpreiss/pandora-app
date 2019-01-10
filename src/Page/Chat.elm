module Page.Chat exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material
import Material.Options as Options
import Material.Button as Button
import Material.Icon as Icon
import Json.Decode as Decode
import Api.Chat as Api
import Util.Port as Port
import Util.Types exposing (Chat, GlobalModel)


-- ΩΩΩ MODEL ΩΩΩ


type alias Model =
    { email : String
    , chatInput : String
    , chats : List Chat
    , username : String
    , newUser : Bool
    , userNameInput : String
    , mdl : Material.Model
    }


init : GlobalModel -> ( Model, Cmd Msg )
init globalModel =
    { email = ""
    , chatInput = ""
    , chats = []
    , username = ""
    , newUser = True
    , userNameInput = ""
    , mdl = Material.model
    }
        ! [ Http.send GotChats (Api.getAll globalModel.authToken) ]



-- ΩΩΩ UPDATE ΩΩΩ


type Msg
    = ChatInput String
    | UserNameInput String
    | SetUserName
    | GotChats (Result Http.Error (List Chat))
    | SendChat
    | SentChat (Result Http.Error String)
    | ChatSocket Chat
      -- Misc
    | NoOp
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChatInput content ->
            { model | chatInput = content }
                ! []

        UserNameInput username ->
            { model | userNameInput = username }
                ! []

        SetUserName ->
            { model
                | username = model.userNameInput
                , newUser = False
            }
                ! [ Port.rememberUsername model.userNameInput
                  , Port.newUser ()
                  ]

        GotChats result ->
            case result of
                Ok chatList ->
                    { model | chats = chatList }
                        ! []

                Err _ ->
                    model ! []

        SendChat ->
            { model | chatInput = "" }
                ! [ Http.send SentChat
                        (Api.send
                            { email = model.email
                            , username =
                                model.username
                            , content = model.chatInput
                            }
                        )
                  ]

        SentChat result ->
            model ! []

        ChatSocket message ->
            { model | chats = message :: model.chats }
                ! []

        NoOp ->
            model ! []

        Mdl msg ->
            Material.update Mdl msg model



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.chatSocket ChatSocket
        , Material.subscriptions Mdl model
        ]



-- ΩΩΩ VIEW ΩΩΩ


viewWindow :
    Model
    -> Html Msg
viewWindow model =
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
                (allChats model.chats)
            ]

        -- CHAT ^^^
        , chatInput model
        , if model.newUser then
            darkEle
          else
            text ""
        , if model.newUser then
            chatLogin model
          else
            text ""
        ]


view : Model -> Html Msg
view model =
    div []
        []


viewChat : Chat -> Html Msg
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


allChats : List Chat -> List (Html Msg)
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


darkEle : Html Msg
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


chatInput : Model -> Html Msg
chatInput model =
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
            , value model.chatInput
            , onInput ChatInput
            , Html.Events.onWithOptions "keydown"
                { stopPropagation = True
                , preventDefault = False
                }
                (Decode.succeed NoOp)
            ]
            []
        , (Button.render Mdl
            [ 7834 ]
            model.mdl
            [ Button.ripple
            , Options.css "height" "100%"
            , Options.css "margin-top" ".45%"
            , Options.onClick SendChat
            ]
            [ text "Send" ]
          )
        ]


chatLogin :
    Model
    -> Html Msg
chatLogin model =
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
            , onInput UserNameInput
            , style
                [ ( "height", "12%" )
                , ( "padding-left", "2%" )
                , ( "font-size", "100%" )
                , ( "margin-top", "auto" )
                ]
            ]
            []
        , (Button.render Mdl
            [ 18 ]
            model.mdl
            [ Button.minifab
            , Button.ripple
            , (if model.userNameInput == "" then
                Options.onClick NoOp
               else
                Options.onClick SetUserName
              )
            , Options.css "height" "20%"
            , Options.css "margin-bottom" "0px"
            ]
            [ Icon.i "arrow_right_alt" ]
          )
        ]
