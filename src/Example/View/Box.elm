module Example.View.Box where

import Graphics.Collage exposing (..)
import Color exposing (..)
import Physics.Types exposing (..)
import Example.View.Config as Conf

render : Body -> Form
render {bounds, position} = let
  bounds' = List.map (bimap <| flip (/) Conf.scale) bounds
  in polygon bounds'
    |> filled blue
    |> moveX (position.x / Conf.scale)
    |> moveY (position.y / Conf.scale)
