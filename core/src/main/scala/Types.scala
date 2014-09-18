package scunits

package object types {

  // IntOps:
  type +[L <: Integer, R <: Integer] = L#Add[R]
  type -[L <: Integer, R <: Integer] = L#Sub[R]
  type _1 = _0#Succ
  type _2 = _1#Succ
  type _3 = _2#Succ
  type _4 = _3#Succ
  type _5 = _4#Succ
  type _6 = _5#Succ
  type _7 = _6#Succ
  type _8 = _7#Succ
  type _9 = _8#Succ

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
}