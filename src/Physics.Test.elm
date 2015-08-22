module Physics.Test where

import Check exposing (..)
import Check.Runner.Browser exposing (display)
import Check.Investigator exposing (..)
import Physics exposing (..)
import Physics.Types exposing (..)
import Physics.Body exposing (momentum)
import Physics.Vector2 exposing (append, magnitude)
import Physics.BodyTest exposing (body)

t : Claim
t = claimTrue "its true" (always True) bool

conservationOfMomentum : Claim
conservationOfMomentum = let
  add : (Body, Body) -> Float
  add (x, y) = momentum x `append` momentum y
    |> magnitude
  noBounce (x, y) =
    ( { x | restitution <- 0 }
    , { y | restitution <- 0 } )
  in claim "deflection conserves momentum"
    `that` (add << deflect << noBounce) `is` add
    `for` tuple (body, body)

main =
  display <| quickCheck conservationOfMomentum
