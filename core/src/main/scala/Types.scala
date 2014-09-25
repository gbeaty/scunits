package scunits

package object types {

  // IntOps:
  type +[L <: Integer, R <: Integer] = L#add[R]
  type -[L <: Integer, R <: Integer] = L#sub[R]
  type i1 = i0#succ
  type i2 = i1#succ
  type i3 = i2#succ
  type i4 = i3#succ
  type i5 = i4#succ
  type i6 = i5#succ
  type i7 = i6#succ
  type i8 = i7#succ
  type i9 = i8#succ

  // CompareOps:
  type ==[L <: Comparable, R <: L#to] = L#compare[R]#isEqual
  type <>[L <: Comparable, R <: L#to] = L#compare[R]#isEqual#Neg
  type >[L <: Comparable, R <: L#to] = L#compare[R]#isGreater
  type <[L <: Comparable, R <: L#to] = L#compare[R]#isLess
  type >=[L <: Comparable, R <: L#to] = L#compare[R]#isGreaterOrEqual
  type <=[L <: Comparable, R <: L#to] = L#compare[R]#isLessOrEqual

  // BoolOps:
  type ||[L <: Bool, R <: Bool] = L#Or[R]
  type &&[L <: Bool, R <: Bool] = L#And[R]

  // DimsOps:
  type *[L <: Dims, R <: DimsOf[L#Quants]] = L#mult[R]
  type /[L <: Dims, R <: DimsOf[L#Quants]] = L#div[R]
}