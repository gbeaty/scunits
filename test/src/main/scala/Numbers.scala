package scunits.test

import scunits.types._

object Integer {
  implicitly[i0 + i0 =:= i0]
  implicitly[i0 - i0 =:= i0]
  implicitly[i1 + i2 =:= i3]
  implicitly[i1 - i2 =:= i1#Neg]
  implicitly[i1#Neg + i2 =:= i1]
  implicitly[i1#Neg + i2#Neg =:= i3#Neg]

  implicitly[(i0 == i0) =:= True]
  implicitly[(i4 == i4) =:= True]
  implicitly[(i1#Neg == i1#Neg) =:= True]

  implicitly[(i3#Neg < i2) =:= True]
  implicitly[(i3 < i2#Neg) =:= False]
  implicitly[(i2 < i2) =:= False]

  implicitly[i0#AddNN[i0] =:= i0]
  implicitly[i1#AddNN[i3] =:= i4]
  implicitly[i1#Neg#AddNN[i3] =:= i2]
  implicitly[i2#AddNP[i0] =:= i2]
  implicitly[i1#Neg#AddNP[i4#Neg] =:= i5#Neg]

  implicitly[i1#IsPos =:= True]
  implicitly[i0#IsPos =:= False]
  implicitly[i1#Neg#IsPos =:= False]

  // implicitly[(_3#Neg > _2) =:= True]
  // implicitly[(_3 > _2#Neg) =:= False]
  // implicitly[(_2 > _2) =:= False]
}