module Example.Main where

import Window exposing (dimensions)
import Signal exposing (..)
import Time exposing (fps, Time, inSeconds)
import Example.View exposing (view)
import Example.Types exposing (..)

import Physics exposing (..)
import Physics.Types exposing (..)
import Physics.Body as Body
import Physics.Transform as Transform

import Debug as D

gravity = Force 0 -9.89

step : Time -> State -> State
step t (State total b) =
  Body.step (D.watch "Delta" t) b
  |> give gravity
  |> D.watch "Body"
  |> State (D.watch "Time" (total + inSeconds t))

initial : State
initial = State 0
  { position     = Transform 0 4 0
  , velocity     = Transform.empty
  , acceleration = Transform.empty
  , mass         = 1 }

main =
  view <~ dimensions
        ~ foldp step initial (fps 30)
