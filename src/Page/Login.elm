module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- ΩΩΩ MODEL ΩΩΩ


type alias Model =
    { messages : List String
    }


init : ( Model, Cmd Msg )
init =
    { messages = []
    }
        ! []



-- ΩΩΩ UPDATE ΩΩΩ


type Msg
    = AddMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddMessage ->
            model ! []



-- ΩΩΩ SUBSCRIPTIONS ΩΩΩ


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- ΩΩΩ VIEW ΩΩΩ


view : Model -> Html Msg
view model =
    div []
        []
