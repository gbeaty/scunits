package scunits.types

import scunits._
import TestTypes._

object DListTests {
  // Test Head:
  implicitly[ENil#Head =:= i0]
  implicitly[i3003#Head =:= i3]

  // Test neg:
  implicitly[ENil#neg =:= ENil]
  implicitly[i012#neg =:= (i0 *: i1#neg *: i2#neg *: ENil)]

  // Test TruncZeros:
  implicitly[i00#TruncZeros =:= ENil]
  implicitly[(i1 *: i0 *: i2 *: i0 *: ENil)#TruncZeros =:= (i1 *: i0 *: i2 *: ENil)]

  // Test Get:
  implicitly[ENil#Get[i2] =:= i0]
  implicitly[i3003#Get[i0] =:= i3]
  implicitly[i3003#Get[i1] =:= i0]
  implicitly[i3003#Get[i2] =:= i0]
  implicitly[i3003#Get[i3] =:= i3]
  implicitly[i3003#Get[i4] =:= i0]

  // Test Set:
  implicitly[ENil#Set[i0,i1] =:= (i1 *: ENil)]
  implicitly[ENil#Set[i1,i3] =:= (i0 *: i3 *: ENil)]
  implicitly[ENil#Set[i2,i1] =:= (i0 *: i0 *: i1 *: ENil)]
  implicitly[ENil#Set[i4,i4] =:= (i0 *: i0 *: i0 *: i0 *: i4 *: ENil)]

  // Test mult:
  implicitly[i11#mult[ENil] =:= i11]
  implicitly[ENil#mult[i11] =:= i11]
  implicitly[ENil#mult[ENil] =:= ENil]
  implicitly[i012#mult[i3003] =:= (i3 *: i1 *: i2 *: i3 *: ENil)]
  implicitly[i3003#neg#mult[i3003] =:= ENil]

  // Test div:
  implicitly[i012#div[ENil] =:= i012]
  implicitly[ENil#div[ENil] =:= ENil]
  implicitly[i012#div[i012] =:= ENil]
  implicitly[i012#div[i3003] =:= (i3#neg *: i1 *: i2 *: i3#neg *: ENil)]
  implicitly[ENil#div[i012] =:= i012#neg]
}