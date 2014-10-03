package scunits.test

import scunits._
import scunits.integer._
import scunits.integer.Ops._

object Integer {
  type p5 = p4#Succ
  type p6 = p5#Succ
  type p7 = p6#Succ
  type p8 = p7#Succ
  type p9 = p8#Succ
  
  // Addition:
  implicitly[_0 + _0 =:= _0]  
  implicitly[p1 + p2 =:= p3]  
  implicitly[n1 + p2 =:= p1]
  implicitly[n1 + n2 =:= n3]

  // Subtraction:
  implicitly[_0 - _0 =:= _0]
  implicitly[p1 - p2 =:= n1]

  // Multiplication:
  implicitly[_0 * _0 =:= _0]
  implicitly[p5 * _0 =:= _0]
  implicitly[_0 * p3 =:= _0]
  implicitly[p1 * p4 =:= p4]
  implicitly[p3 * p3 =:= p9]
  implicitly[p2 * p2 =:= p4]
}