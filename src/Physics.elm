module Physics where

import Focus exposing (..)
import Physics.Types exposing (..)
import Physics.Body as Body
import Physics.Force exposing (div, append)
import Physics.Transform as Transform

density = 1.2754

give : Force -> Body -> Body
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
  in (          f (velocity => x)  body  body'
       |> flip (f (velocity => y))       body'
     ,          f (velocity => x)  body' body
       |> flip (f (velocity => y))       body )
