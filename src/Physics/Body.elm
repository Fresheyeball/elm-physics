module Physics.Body where

import Focus exposing (..)
import Physics.Types exposing (..)
import Physics.Transform as T

move : Transform -> Body -> Body
move delta = update position <| T.move delta

rotate : Float -> Body -> Body
rotate delta = update position <| T.rotate delta

empty = Body T.empty T.empty
