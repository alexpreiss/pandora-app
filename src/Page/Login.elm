module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Material
import Material.Options as Options
import Material.Button as Button
import Util.Types exposing (GlobalModel, cmds)
import Api.Login as AuthApi
import Api.Station as StationApi
import Util.Port as Port
import Util.Types as Types exposing (Station, LoginModel, Page(..))


-- ΩΩΩ MODEL ΩΩΩ


type alias Flags =
    { email : Maybe String
    , password : Maybe String
    }


init : Flags -> ( LoginModel, Cmd Msg )
init { email, password } =
    { email =
        (case email of
            Just email ->
                email

            Nothing ->
                ""
        )
    , password =
        (case password of
            Just password ->
                password

            Nothing ->
                ""
        )
    , remember = False
    , failed = False
    }
        ! []



-- ΩΩΩ UPDATE ΩΩΩ


type Msg
    = InputEmail String
    | InputPassword String
    | Login
    | LoginRemember
    | RememberMe Bool
    | LoggedIn (Result Http.Error String)
    | LoggedInRemember (Result Http.Error String)
    | GotStations (Result Http.Error (List Station))
    | Mdl (Material.Msg Msg)


update : Msg -> LoginModel -> GlobalModel -> ( LoginModel, GlobalModel, Cmd Msg )
update msg model globalModel =
    case msg of
        InputEmail input ->
            ( { model | email = input }
            , globalModel
            , cmds []
            )

        InputPassword input ->
            ( { model | password = input }
            , globalModel
            , cmds []
            )

        Login ->
            ( model
            , globalModel
            , cmds
                [ Http.send LoggedIn
                    (AuthApi.login
                        { email = model.email
                        , password = model.password
                        }
                    )
                ]
            )

        LoginRemember ->
            ( model
            , globalModel
            , cmds
                [ Http.send LoggedInRemember
                    (AuthApi.login
                        { email = model.email
                        , password = model.password
                        }
                    )
                , Port.rememberEmail model.email
                , Port.rememberPassword model.password
                ]
            )

        LoggedInRemember result ->
            case result of
                Ok token ->
                    ( model
                    , { globalModel
                        | authToken = token
                        , page = StationSelector
                      }
                    , cmds
                        [ Http.send GotStations
                            (StationApi.get token)
                        , Port.rememberPassword model.password
                        , Port.rememberEmail model.email
                        ]
                    )

                Err error ->
                    ( { model | failed = True }, globalModel, cmds [] )

        LoggedIn result ->
            case result of
                Ok token ->
                    ( model
                    , { globalModel
                        | authToken = token
                        , page = StationSelector
                      }
                    , cmds
                        [ Http.send
                            GotStations
                            (StationApi.get token)
                        ]
                    )

                Err error ->
                    ( { model | failed = True }, globalModel, cmds [] )

        GotStations result ->
            ( model
            , case result of
                Ok stations ->
                    { globalModel
                        | stations = stations
                    }

                Err error ->
                    globalModel
            , cmds []
            )

        Mdl msg ->
            let
                ( gm, cmd ) =
                    Material.update Mdl msg globalModel
            in
                ( model, gm, cmd )

        RememberMe bool ->
            ( model
            , globalModel
            , cmds
                [ Port.rememberEmail model.email
                , Port.rememberPassword model.password
                ]
            )



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : LoginModel -> Sub Msg
subscriptions _ =
    Sub.none



-- ΩΩΩ VIEW ΩΩΩ


view : GlobalModel -> Html Msg
view gm =
    let
        model =
            gm.loginModel
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "width", "650px" )
                , ( "height", "100%" )
                , ( "margin", "auto" )
                , ( "margin-top", "10px" )
                , ( "padding-top", "30px" )
                , ( "padding-bottom", "30px" )
                ]
            ]
            [ h3
                [ style
                    [ ( "margin-top", "0" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ text "Log in" ]
            , input
                [ style
                    [ ( "margin-bottom", "30px" )
                    , ( "margin-top", "25px" )
                    , ( "width", "450px" )
                    , ( "align-self", "center" )
                    , ( "height", "35px" )
                    ]
                , type_ "email"
                , placeholder "Email"
                , onInput InputEmail
                ]
                []
            , input
                [ type_ "password"
                , placeholder "Password"
                , onInput InputPassword
                , style
                    [ ( "width", "450px" )
                    , ( "align-self", "center" )
                    , ( "height", "35px" )
                    , ( "margin-bottom", "30px" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "margin-top", "10%" )
                    , ( "height", "40px" )
                    , ( "width", "460px" )
                    , ( "align-self", "center" )
                    , ( "display", "flex" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "display", "flex" )
                        , ( "margin-left", "10px" )
                        , ( "margin-right", "auto" )
                        , ( "margin-top", "10px" )
                        ]
                    ]
                    [ p [] [ text "Remember me" ]
                    , input
                        [ type_ "checkbox"
                        , onCheck RememberMe
                        , style
                            [ ( "margin-top", "7px" )
                            , ( "margin-left", "5px" )
                            ]
                        ]
                        []
                    ]
                , Button.render Mdl
                    [ 102321 ]
                    gm.mdl
                    [ if model.remember then
                        Options.onClick LoginRemember
                      else
                        Options.onClick Login
                    , Button.raised
                    , Button.ripple
                    , Options.css "margin-right" "10px"
                    , Options.css "margin-left" "auto"
                    ]
                    [ text "Log in" ]
                ]
            , if model.failed then
                p
                    [ style
                        [ ( "color", "red" )
                        , ( "text-align", "center" )
                        ]
                    ]
                    [ text "Invalid username or password" ]
              else
                text ""
            ]
