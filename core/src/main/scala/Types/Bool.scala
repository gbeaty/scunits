package scunits.types

trait Bool {
  type If[B,T <: B, E <: B] <: B
  type Neg <: Bool
  type Or[R <: Bool] <: Bool
  type And[R <: Bool] <: Bool
  type Xor[R <: Bool] <: Bool
}
trait True extends Bool {
  type Neg = False
  type If[B,T <: B, E <: B] = T
  type Or[R <: Bool] = True
  type And[R <: Bool] = R
  type Xor[R <: Bool] = R#Neg
}
trait False extends Bool {
  type Neg = True
  type If[B,T <: B, E <: B] = E
  type Or[R <: Bool] = R
  type And[R <: Bool] = False
  type Xor[R <: Bool] = R
}