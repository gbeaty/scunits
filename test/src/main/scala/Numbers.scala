package scunits.test

import scunits._
import scunits.integer._
import scunits.integer.Ops._

object Integer {
  // Addition:
  implicitly[_0 + _0 =:= _0]  
  implicitly[_1 + _2 =:= _3]  
  implicitly[_1#Neg + _2 =:= _1]
  implicitly[_1#Neg + _2#Neg =:= _3#Neg]

  // Subtraction:
  implicitly[_0 - _0 =:= _0]
  implicitly[_1 - _2 =:= _1#Neg]

  // Multiplication:
  implicitly[_0 * _0 =:= _0]
  implicitly[_5 * _0 =:= _0]
  implicitly[_0 * _3 =:= _0]
  implicitly[_1 * _4 =:= _4]
  implicitly[_3 * _3 =:= _9]
  implicitly[_2 * _2 =:= _4]
}