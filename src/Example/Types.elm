module Example.Types where

import Graphics.Collage exposing (..)
import Physics.Types exposing (..)
import Time exposing (..)

type State =
  State Time (List Object)

type alias Object =
  { body   : Body
  , render : (Body -> Form) }
