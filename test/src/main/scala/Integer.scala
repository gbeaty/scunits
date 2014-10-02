package scunits.test

import scunits.types._

object Integer {
  implicitly[i0 + i0 =:= i0]
  implicitly[i0 - i0 =:= i0]
  implicitly[i1 + i2 =:= i3]
  implicitly[i1 - i2 =:= i1#neg]
  implicitly[i1#neg + i2 =:= i1]
  implicitly[i1#neg + i2#neg =:= i3#neg]

  implicitly[i1#isPos =:= True]
  implicitly[i0#isPos =:= False]
  implicitly[i1#neg#isPos =:= False]

  type plusPlus[I <: Integer] = I#succ
  type double[I <: Integer] = I#loop[Integer,plusPlus,I]
  implicitly[double[i1] =:= i2]
  implicitly[double[i2] =:= i4]
  implicitly[double[i4] =:= i8]
}