module Physics where

import Focus exposing (..)
import Physics.Types exposing (..)
import Physics.Body as Body
import Physics.Vector2 exposing (div, append)
import Physics.Transform as Transform
import Physics.Collision as Collision

import Debug

density = 1.2754

gravity : Body -> Body
gravity b =
  { b | acceleration <- b.acceleration `append` Vector2 0 -9.86 }

give : Vector2 -> Body -> Body
give f b = let
  f' = f `div` .mass b
  g a = { a | x <- a.x + f'.x
            , y <- a.y + f'.y }
  in update acceleration g b

deflect : (Body, Body) -> (Body, Body)
deflect (body, body') = let
  f : Focus Body Float -> Body -> Body -> Body
  f l a b = let
    v = let
      cr = body.restitution + body'.restitution
         |> clamp 0 1
      in cr * a.mass * (get l b - get l a)
        + b.mass  *  get l b
        + a.mass  *  get l a
        / b.mass  +  a.mass
    in update l (always v) a
  g bx by = f (velocity => x) bx by
     |> flip (f (velocity => y)) by
  in (g body body', g body' body)


-- collisions : List Body -> List Body
-- collisions bs = let
--   (<$>)  = List.map
--   (>>=)  = flip List.concatMap
