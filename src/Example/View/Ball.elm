module Example.View.Ball where

import Graphics.Collage exposing (..)
import Color exposing (..)
import Physics.Types exposing (..)
import Example.View.Config as Conf

render : Body -> Form
render b = circle 10
  |> filled red
  |> moveX (b.position.x / Conf.scale)
  |> moveY (b.position.y / Conf.scale)
