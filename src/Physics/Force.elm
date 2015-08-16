module Physics.Force where

import Physics.Types exposing (..)

empty : Force
empty = Force 0 0

append : Force -> Force -> Force
append f f' = Force
  (f.x + f'.x)
  (f.y + f'.y)

concat : List Force -> Force
concat = List.foldr append empty

div : Force -> Float -> Force
div f n = Force
  (f.x / n)
  (f.y / n)
