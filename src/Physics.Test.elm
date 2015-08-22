module Physics.Test where

import Check exposing (..)
import Check.Runner.Browser exposing (display)
import Check.Investigator exposing (bool)
import Physics exposing (..)

t : Claim
t = claimTrue "its true" (always True) bool

-- conservationOfMomentum : Claim
-- conservationOfMomentum =


main =
  display <| quickCheck t
