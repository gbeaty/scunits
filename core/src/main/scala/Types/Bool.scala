package scunits.types

sealed class Bool {
  type branch[B,T <: B, E <: B] <: B
  type not <: Bool
  type or[R <: Bool] <: Bool
  type and[R <: Bool] <: Bool
  type xor[R <: Bool] <: Bool
}
final class True extends Bool {
  type branch[B,T <: B, E <: B] = T
  type not = False  
  type or[R <: Bool] = True
  type and[R <: Bool] = R
  type xor[R <: Bool] = R#not
}
final class False extends Bool {
  type branch[B,T <: B, E <: B] = E
  type not = True  
  type or[R <: Bool] = R
  type and[R <: Bool] = False
  type xor[R <: Bool] = R
}