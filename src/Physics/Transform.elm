module Physics.Transform where

import Physics.Types exposing (..)
import Focus exposing (create, get, Focus, update)

empty : Transform
empty = Transform 0 0 (turns 0)

trimap : (Float -> Float) -> Transform -> Transform
trimap f {x,y,r} =
  { x = f x
  , y = f y
  , r = rmod (f r) }

combineWith : (Float -> Float -> Float)
  -> Transform -> Transform -> Transform
combineWith o t t' = Transform
  (t.x `o` t'.x)
  (t.y `o` t'.y)
  (rmod (t.r `o` t'.r))

append = combineWith (+)

concat : List Transform -> Transform
concat = List.foldr append empty

move : { record | x : Float, y : Float} -> Transform -> Transform
move delta t =
  { t | x <- (delta.x + t.x)
      , y <- (delta.y + t.y) }

rotate : Angle -> Transform -> Transform
rotate delta =
  (+) delta >> rmod |> update r
