module Example.Main where

import Window exposing (dimensions)
import Signal exposing (..)
import Time exposing (fps, Time)
import Example.View exposing (view)

import Physics exposing (..)
import Physics.Types exposing (..)
import Physics.Body as Body
import Physics.Transform as Transform

import Debug as D

gravity = Force 0 -0.989

step : Time -> Body -> Body
step t b = give gravity <|
  Body.step
  (D.watch "Time" t)
  (D.watch "Body" b)

initial : Body
initial =
  { position     = Transform 1 3 0
  , velocity     = Transform.empty
  , acceleration = Transform.empty
  , mass         = 1 }

main =
  view <~ dimensions
        ~ foldp step initial (fps 30)
