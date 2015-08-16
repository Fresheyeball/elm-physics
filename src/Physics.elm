module Physics where

import Focus exposing (..)
import Physics.Types exposing (..)
import Physics.Body as Body
import Physics.Force exposing (div, append)
import Physics.Transform as Transform

density = 1.2754

give : Force -> Body -> Body
give f b = let
  f' = f `div` .mass b
  g a = { a | x <- a.x + f'.x
            , y <- a.y + f'.y }
  in update acceleration g b
