package scunits.types

import scunits._
import TestTypes._

object DListTests {
  // Test Head:
  implicitly[DNil#Head =:= i0]
  implicitly[i3003#Head =:= i3]

  // Test neg:
  implicitly[DNil#neg =:= DNil]
  implicitly[i012#neg =:= (i0 *: i1#neg *: i2#neg *: DNil)]

  // Test TruncZeros:
  implicitly[i00#TruncZeros =:= DNil]
  implicitly[(i1 *: i0 *: i2 *: i0 *: DNil)#TruncZeros =:= (i1 *: i0 *: i2 *: DNil)]

  // Test Get:
  implicitly[DNil#Get[i2] =:= i0]
  implicitly[i3003#Get[i0] =:= i3]
  implicitly[i3003#Get[i1] =:= i0]
  implicitly[i3003#Get[i2] =:= i0]
  implicitly[i3003#Get[i3] =:= i3]
  implicitly[i3003#Get[i4] =:= i0]

  // Test Set:
  implicitly[DNil#Set[i0,i1] =:= (i1 *: DNil)]
  implicitly[DNil#Set[i1,i3] =:= (i0 *: i3 *: DNil)]
  implicitly[DNil#Set[i2,i1] =:= (i0 *: i0 *: i1 *: DNil)]
  implicitly[DNil#Set[i4,i4] =:= (i0 *: i0 *: i0 *: i0 *: i4 *: DNil)]

  // Test mult:
  implicitly[i11#mult[DNil] =:= i11]
  implicitly[DNil#mult[i11] =:= i11]
  implicitly[DNil#mult[DNil] =:= DNil]
  implicitly[i012#mult[i3003] =:= (i3 *: i1 *: i2 *: i3 *: DNil)]
  implicitly[i3003#neg#mult[i3003] =:= DNil]

  // Test div:
  implicitly[i012#div[DNil] =:= i012]
  implicitly[DNil#div[DNil] =:= DNil]
  implicitly[i012#div[i012] =:= DNil]
  implicitly[i012#div[i3003] =:= (i3#neg *: i1 *: i2 *: i3#neg *: DNil)]
  implicitly[DNil#div[i012] =:= i012#neg]
}