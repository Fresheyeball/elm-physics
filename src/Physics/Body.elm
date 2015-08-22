module Physics.Body where

import Focus exposing (..)
import Time exposing (Time, inSeconds)
import Physics.Types exposing (..)
import Physics.Transform as T
import Physics.Vector2 as V

move : Transform -> Body -> Body
move delta = Focus.update position (T.move delta)

rotate : Float -> Body -> Body
rotate delta = Focus.update position <| T.rotate delta

empty = Body T.empty T.empty T.empty 0

step : Time -> Body -> Body
step delta b = let
  trans ** float = T.map ((*) float) trans
  (++) = T.append
  delta' = inSeconds delta
  v = b.velocity ++ (b.acceleration ** delta')
  p = b.position ++ (v ** delta')
  in { b | velocity     <- v
         , position     <- p
         , acceleration <- T.empty }

momentum : Body -> Vector2
momentum b =
  (+) b.mass `V.map` b.velocity
