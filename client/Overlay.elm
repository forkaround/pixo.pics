module Overlay exposing (glass, red, solid)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


glass : Attribute msg
glass =
    class "border-transparent! bg-white/20! text-white! backdrop-blur-sm hover:bg-white/35!"


solid : Attribute msg
solid =
    class "border-transparent! bg-white! text-black! hover:bg-white/85!"


red : Attribute msg
red =
    class "border-transparent! bg-red-600! text-white! hover:bg-red-700!"
