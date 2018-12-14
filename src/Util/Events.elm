module Util.Events exposing (blockSpacebar)

import Json.Decode as Decode
import Html
import Html.Events exposing (..)


blockSpacebar : msg -> Html.Attribute msg
blockSpacebar noOp =
    Html.Events.onWithOptions "keydown"
        { stopPropagation = True
        , preventDefault = False
        }
        (Decode.succeed noOp)
