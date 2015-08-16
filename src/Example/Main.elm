module Example.Main where

import Window as W
import Signal exposing (..)
import Time exposing (fps)
import Example.View exposing (view)


main = view <~ W.dimensions ~ fps 30
