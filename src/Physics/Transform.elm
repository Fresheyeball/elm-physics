module Physics.Transform where

import Physics.Types exposing (..)
import Focus exposing (create, get, Focus, update)

empty : Transform
empty = Transform 0.0 0.0 (turns 1)

append : Transform -> Transform -> Transform
append t t' = let
  addOn f = get f t + get f t'

  in { x = addOn x
     , y = addOn y
     , r = addOn r |> rmod }

concat : List Transform -> Transform
concat = List.foldr append empty

move : { record | x : Float, y : Float} -> Transform -> Transform
move delta t =
  { t | x <- (delta.x + t.x)
      , y <- (delta.y + t.y) }

rotate : Angle -> Transform -> Transform
rotate delta =
  (+) delta >> rmod |> update r
