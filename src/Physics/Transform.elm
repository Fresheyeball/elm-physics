module Physics.Transform where

import Physics.Types exposing (..)
import Focus exposing (create, get, Focus, update)

empty : Transform
empty = Transform 0 0 (turns 0)

map : (Float -> Float) -> Transform -> Transform
map f {x,y,r} =
  { x = f x
  , y = f y
  , r = rmod (f r) }

map2 : (Float -> Float -> Float)
  -> Transform -> Transform -> Transform
map2 o t t' = Transform
  (t.x `o` t'.x)
  (t.y `o` t'.y)
  (rmod (t.r `o` t'.r))

append = map2 (+)

concat : List Transform -> Transform
concat = List.foldr append empty

move : { record | x : Float, y : Float} -> Transform -> Transform
move delta t =
  { t | x <- (delta.x + t.x)
      , y <- (delta.y + t.y) }

rotate : Angle -> Transform -> Transform
rotate delta =
  (+) delta >> rmod |> update r
