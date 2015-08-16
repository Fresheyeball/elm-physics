module Debug.Grid where

import Graphics.Collage exposing (..)
import Color exposing (blue, green)

renderCell : Float -> (Int, Int) -> (Int, Int) -> Form
renderCell d (w, h) (x, y) = let
  conv a = toFloat a * d
  r      = d / 2
  in rect d d
  |> outlined (solid blue)
  >> move (conv x - r, conv y + r)

renderGrid : (Int, Int) -> Float -> List Form
renderGrid (w, h) scale = let

  dimension    = 1.0 / scale
  rowLength    = ((+) 2) << ceiling <| toFloat w / dimension
  columnHeight = ((+) 2) << ceiling <| toFloat h / dimension
  (<$>) = List.map

  grid : List (Int, Int)
  grid = let
    row x = List.repeat rowLength (flip (,) x)
    |> List.indexedMap (|>)
    in List.concat
     <| List.indexedMap
      (always << row)
      (List.repeat columnHeight [])

  shiftCenter : (Int, Int) -> (Int, Int)
  shiftCenter (x, y) = let
      a // b = round <| toFloat a / b
      shift a l = if a > l // 2 then a - l else a
    in (shift x rowLength, shift y columnHeight)

  in renderCell dimension (w, h) <$> (shiftCenter <$> grid)

renderOrigin : Form
renderOrigin =
  filled green <| circle 5.0

renderAxes : (Int, Int) -> List Form
renderAxes (w, h) = let
  filled' c x y = filled c <| rect x y
  w' = toFloat w
  h' = toFloat h
  in [ filled' green 1.0 h'
     , filled' green w' 1.0 ]

render : (Int, Int) -> Float -> Form
render d scale = group <| renderGrid d scale ++ renderAxes d ++ [renderOrigin]
