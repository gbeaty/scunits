package scunits

package object types {

  // IntOps:
  type +[L <: Integer, R <: Integer] = L#Add[R]
  type -[L <: Integer, R <: Integer] = L#Sub[R]
  type i1 = i0#Succ
  type i2 = i1#Succ
  type i3 = i2#Succ
  type i4 = i3#Succ
  type i5 = i4#Succ
  type i6 = i5#Succ
  type i7 = i6#Succ
  type i8 = i7#Succ
  type i9 = i8#Succ

  // CompareOps:
  type ==[L <: Comparable, R <: L#With] = L#Compare[R]#IsEqual
  type <>[L <: Comparable, R <: L#With] = L#Compare[R]#IsEqual#Neg
  type >[L <: Comparable, R <: L#With] = L#Compare[R]#IsGreater
  type <[L <: Comparable, R <: L#With] = L#Compare[R]#IsLess
  type >=[L <: Comparable, R <: L#With] = L#Compare[R]#IsGreaterOrEqual
  type <=[L <: Comparable, R <: L#With] = L#Compare[R]#IsLessOrEqual

  // BoolOps:
  type ||[L <: Bool, R <: Bool] = L#Or[R]
  type &&[L <: Bool, R <: Bool] = L#And[R]

  // DimsOps:
  type *[L <: Dims, R <: DimsOf[L#Quants]] = L#Mult[R]
  type /[L <: Dims, R <: DimsOf[L#Quants]] = L#Div[R]
}