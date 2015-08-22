module Physics.TransformTest where

import Shrink
import Random.Extra as Random
import Physics.Types exposing(..)
import Check.Investigator exposing (..)

transform : Investigator Transform
transform = let
  shrinker t = Transform
    `Shrink.map`    shrink float t.x
    `Shrink.andMap` shrink float t.y
    `Shrink.andMap` shrink float t.r
  generator = Transform
    `Random.map`    random float
    `Random.andMap` random float
    `Random.andMap` (rmod `Random.map` random float)
  in investigator generator shrinker
