module Strings where


data String = String Length Tension

stringVelocity :: Tension -> LinearDensity -> Velocity
stringVelocity = undefined


type Tension = Float
type LinearDensity = Float
type Length = Float
type Velocity = Float
