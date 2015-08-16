module Example.View where

import Graphics.Collage exposing (collage, Form)
import Graphics.Element exposing (color, Element)
import Debug.Grid as Grid
import Time exposing (Time)
import Color
import Physics.Types exposing (..)
import Example.View.Config as Conf
import Example.View.Ball as Ball

gridWith : (Int, Int) -> List Form -> Element
gridWith (w, h) =
  (::) (Grid.render (w, h) Conf.scale)
  >> collage w h
  >> color Color.black

view : (Int, Int) -> Body -> Element
view (w, h) b = gridWith (w, h) [ Ball.render b ]
