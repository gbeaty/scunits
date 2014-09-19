package scunits.test

import scunits.types._

object Integer {
  implicitly[_0 + _0 =:= _0]
  implicitly[_0 - _0 =:= _0]
  implicitly[_1 + _2 =:= _3]
  implicitly[_1 - _2 =:= _1#Neg]
  implicitly[_1#Neg + _2 =:= _1]
  implicitly[_1#Neg + _2#Neg =:= _3#Neg]

  implicitly[(_0 == _0) =:= True]
  implicitly[(_4 == _4) =:= True]
  implicitly[(_1#Neg == _1#Neg) =:= True]

  implicitly[(_3#Neg < _2) =:= True]
  implicitly[(_3 < _2#Neg) =:= False]
  implicitly[(_2 < _2) =:= False]

  implicitly[_0#AddNN[_0] =:= _0]
  implicitly[_1#AddNN[_3] =:= _4]
  implicitly[_1#Neg#AddNN[_3] =:= _2]
  implicitly[_2#AddNP[_0] =:= _2]
  implicitly[_1#Neg#AddNP[_4#Neg] =:= _5#Neg]

  // implicitly[(_3#Neg > _2) =:= True]
  // implicitly[(_3 > _2#Neg) =:= False]
  // implicitly[(_2 > _2) =:= False]
}