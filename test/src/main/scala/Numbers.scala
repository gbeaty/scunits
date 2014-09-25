package scunits.test

import scunits.types._

object Integer {
  implicitly[i0 + i0 =:= i0]
  implicitly[i0 - i0 =:= i0]
  implicitly[i1 + i2 =:= i3]
  implicitly[i1 - i2 =:= i1#neg]
  implicitly[i1#neg + i2 =:= i1]
  implicitly[i1#neg + i2#neg =:= i3#neg]

  implicitly[(i0 == i0) =:= True]
  implicitly[(i4 == i4) =:= True]
  implicitly[(i1#neg == i1#neg) =:= True]

  implicitly[(i3#neg < i2) =:= True]
  implicitly[(i3 < i2#neg) =:= False]
  implicitly[(i2 < i2) =:= False]

  implicitly[i0#addNN[i0] =:= i0]
  implicitly[i1#addNN[i3] =:= i4]
  implicitly[i1#neg#addNN[i3] =:= i2]
  implicitly[i2#addNP[i0] =:= i2]
  implicitly[i1#neg#addNP[i4#neg] =:= i5#neg]

  implicitly[i1#isPos =:= True]
  implicitly[i0#isPos =:= False]
  implicitly[i1#neg#isPos =:= False]

  // implicitly[(_3#neg > _2) =:= True]
  // implicitly[(_3 > _2#neg) =:= False]
  // implicitly[(_2 > _2) =:= False]
}