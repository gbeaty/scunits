package scunits.types

trait Bool {
  type branch[B,T <: B, E <: B] <: B
  type not <: Bool
  type or[R <: Bool] <: Bool
  type and[R <: Bool] <: Bool
  type xor[R <: Bool] <: Bool
}
trait True extends Bool {
  type not = False
  type branch[B,T <: B, E <: B] = T
  type or[R <: Bool] = True
  type and[R <: Bool] = R
  type xor[R <: Bool] = R#not
}
trait False extends Bool {
  type not = True
  type branch[B,T <: B, E <: B] = E
  type or[R <: Bool] = R
  type and[R <: Bool] = False
  type xor[R <: Bool] = R
}