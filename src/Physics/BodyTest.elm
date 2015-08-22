module Physics.BodyTest where

import Shrink
import Random.Extra as Random
import Physics.Types exposing(..)
import Physics.TransformTest exposing (transform)
import Check.Investigator exposing (..)

body : Investigator Body
body = let
  shrinker b = Body
    `Shrink.map`    shrink transform b.position
    `Shrink.andMap` shrink transform b.velocity
    `Shrink.andMap` shrink transform b.acceleration
    `Shrink.andMap` shrink float b.restitution
    `Shrink.andMap` shrink float b.mass
  generator = Body
    `Random.map`    random transform
    `Random.andMap` random transform
    `Random.andMap` random transform
    `Random.andMap` random float
    `Random.andMap` random float
  in investigator generator shrinker
