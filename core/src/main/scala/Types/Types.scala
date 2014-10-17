package scunits

package object types {
  type Dimless = Dims2Const[ANil,Quant]
  
  // Dims2 ops:
  type *[L <: Dims2, R <: Dims2] = op[L,R,+]
  type /[L <: Dims2, R <: Dims2] = op[L,R,-]

  type op[L <: Dims2, R <: Dims2, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type ms = L#accessors#append[R#accessors]
    type base = ms#dimlessQuant
    type res = ms#op[base with L#quant, base with R#quant]#apply[O]
  })#res
  // I'd like to move this to Dims#op, but when they are placed there an odd bug seems to scramble refined type overloading.

  // Integer ops:
  type +[L <: Integer, R <: Integer] = L#Add[R]
  type -[L <: Integer, R <: Integer] = L#Sub[R]

  type p1 = _0#Succ
  type p2 = p1#Succ
  type p3 = p2#Succ
  type p4 = p3#Succ
  type p5 = p4#Succ
  type p6 = p5#Succ
  type p7 = p6#Succ
  type p8 = p7#Succ
  type p9 = p8#Succ

  type n1 = _0#Pred
  type n2 = n1#Pred
  type n3 = n2#Pred
  type n4 = n3#Pred
  type n5 = n4#Pred
  type n6 = n5#Pred
  type n7 = n6#Pred
  type n8 = n7#Pred
  type n9 = n8#Pred
}