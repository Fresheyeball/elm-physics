module Example.View where

import Graphics.Collage exposing (collage, Form)
import Graphics.Element exposing (color, Element)
import Debug.Grid as Grid
import Time exposing (Time)
import Color

import Example.View.Ball as Ball

gridWith : (Int, Int) -> List Form -> Element
gridWith (w, h) =
  (::) (Grid.render (w, h) (1 / 100))
  >> collage w h
  >> color Color.black

view : (Int, Int) -> Time -> Element
view (w, h) _ = gridWith (w, h) []
