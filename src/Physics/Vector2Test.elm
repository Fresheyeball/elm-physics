module Physics.Vector2Test where

import Shrink
import Random.Extra as Random
import Physics.Types exposing(..)
import Check.Investigator exposing (..)

vector2 : Investigator Vector2
vector2 = let
  shrinker v = Vector2
    `Shrink.map` shrink float v.x
    `Shrink.andMap` shrink float v.y
  generator = Vector2
    `Random.map` random float
    `Random.andMap` random float
  in investigator generator shrinker
