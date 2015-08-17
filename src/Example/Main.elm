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
import Example.View.Ball as Ball
import Example.View.Box as Box
import List.Extra as List

import Physics.Collision exposing (..)

import Debug as D

gravity = Force 0 -9.89

step : Time -> State -> State
step t (State total os) = let
  (<$>) = List.map

  totalTime = D.watch "Time" (total + inSeconds t)

  applyForces : Object -> Object
  applyForces o = { o | body <-
    Body.step (D.watch "Delta" t) o.body
    |> give gravity }

  collisions' : List Object -> List Object
  collisions' os = let
    bs  = .body <$> os
    f (o, b) = { o | body <- b }

    in f <$> List.zip os (collisions bs)

  in applyForces <$> os
    |> collisions'
    |> D.watch "Objects"
    |> State totalTime

initial : State
initial = State 0
  [ { body =
        { position     = Transform 0 4 0
        , velocity     = Transform.empty
        , acceleration = Transform.empty
        , restitution  = 0
        , bounds       =
          [ (-0.1,  0.1)
          , ( 0.1,  0.1)
          , ( 0.1, -0.1)
          , (-0.1, -0.1) ]
        , mass         = 1 }
    , render = Box.render }
  , { body =
        { position     = Transform 0 -3 0
        , velocity     = Transform.empty
        , acceleration = Transform.empty
        , restitution  = 0
        , bounds       =
          [ (-0.5,  0.5)
          , ( 0.5,  0.5)
          , ( 0.5, -0.5)
          , (-0.5, -0.5) ]
        , mass         = 10000000 }
    , render = Box.render } ]

main =
  view <~ dimensions
        ~ foldp step initial (fps 30)
