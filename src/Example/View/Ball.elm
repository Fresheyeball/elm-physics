module Example.View.Ball where

import Graphics.Collage exposing (..)
import Color exposing (..)
import Physics.Types exposing (..)

render : Body -> Form
render b = circle 10
  |> filled red
  |> moveX b.position.x
  |> moveY b.position.y
