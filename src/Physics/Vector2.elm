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

map : (Float -> Float) -> { record | x : Float, y : Float} -> Vector2
map f v =
  { x = f v.x
  , y = f v.y }

map2 : (Float -> Float -> Float) -> { record | x : Float, y : Float} -> { record | x : Float, y : Float} -> Vector2
map2 f v v' =
  { x = f v.x v'.x
  , y = f v.y v'.y }

magnitude : Vector2 -> Float
magnitude v = let
  v' = map abs v
  in v'.x^2 + v'.y^2 |> sqrt
