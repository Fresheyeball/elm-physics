module Physics.Force where

import Physics.Types exposing (..)

empty : Force
empty = Force 0 0

append : Force -> Force -> Force
append f f' = Force
  (f.x + f'.x)
  (f.y + f'.y)
