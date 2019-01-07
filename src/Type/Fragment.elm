module Type.Fragment exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Http exposing (Request)


{-| Decodes the last item in an array. Returns the default value if the array
has no elements.
-}
