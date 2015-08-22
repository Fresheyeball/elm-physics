module Physics.Vector2 where

import Physics.Types exposing (..)

empty : Vector2
empty = Vector2 0 0

append : { record | x : Float, y : Float} -> Vector2 -> { record | x : Float, y : Float}
append f f' =
  { f | x <- (f.x + f'.x)
      , y <- (f.y + f'.y) }

concat : List Vector2 -> Vector2
concat = List.foldr append empty

div : Vector2 -> Float -> Vector2
div f n = Vector2
  (f.x / n)
  (f.y / n)
