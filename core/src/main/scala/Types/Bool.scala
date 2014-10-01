package scunits.types

trait Bool {
  type If[B,T <: B, E <: B] <: B
  type neg <: Bool
  type or[R <: Bool] <: Bool
  type and[R <: Bool] <: Bool
  type Xor[R <: Bool] <: Bool
}
trait True extends Bool {
  type neg = False
  type If[B,T <: B, E <: B] = T
  type or[R <: Bool] = True
  type and[R <: Bool] = R
  type Xor[R <: Bool] = R#neg
}
trait False extends Bool {
  type neg = True
  type If[B,T <: B, E <: B] = E
  type or[R <: Bool] = R
  type and[R <: Bool] = False
  type Xor[R <: Bool] = R
}