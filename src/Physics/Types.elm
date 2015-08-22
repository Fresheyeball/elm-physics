module Physics.Types where

import Focus exposing (create, get, Focus, update)

type alias Angle       = Float
type alias Mass        = Float
type alias Restitution = Float
type alias Force       = Float

type alias Vector2 =
  { x : Float
  , y : Float }

type alias Transform =
  { x : Float
  , y : Float
  , r : Angle }

type alias Body =
  { position     : Transform
  , velocity     : Transform
  , acceleration : Transform
  , restitution  : Restitution
  , mass         : Mass }

x : Focus { record | x : Float } Float
x = create .x <|
  \f t -> { t | x <- f (.x t) }

y : Focus { record | y : Float } Float
y = create .y <|
  \f t -> { t | y <- f (.y t) }

r : Focus { record | r : Float } Float
r = create .r <|
  \f t -> { t | r <- f (.r t) }

position : Focus Body Transform
position = create .position <|
  \f t -> { t | position <- f (.position t) }

velocity : Focus Body Transform
velocity = create .velocity <|
  \f t -> { t | velocity <- f (.velocity t) }

acceleration : Focus Body Transform
acceleration = create .acceleration <|
  \f t -> { t | acceleration <- f (.acceleration t) }

fmod a b = let
  c = if b < 0
      then toFloat <| ceiling (a / b)
      else toFloat <| floor   (a / b)
  in a - b * c

rmod = flip fmod (turns 1)
