package scunits.types

import scunits._
import TestTypes._

object DListTests {
  // Test Head:
  implicitly[DNil#Head =:= i0]
  implicitly[i3003#Head =:= i3]

  // Test Neg:
  implicitly[DNil#Neg =:= DNil]
  implicitly[i012#Neg =:= (i0 *: i1#Neg *: i2#Neg *: DNil)]

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

  // Test Mult:
  implicitly[i11#Mult[DNil] =:= i11]
  implicitly[DNil#Mult[i11] =:= i11]
  implicitly[DNil#Mult[DNil] =:= DNil]
  implicitly[i012#Mult[i3003] =:= (i3 *: i1 *: i2 *: i3 *: DNil)]
  implicitly[i3003#Neg#Mult[i3003] =:= DNil]

  // Test Div:
  implicitly[i012#Div[DNil] =:= i012]
  implicitly[DNil#Div[DNil] =:= DNil]
  implicitly[i012#Div[i012] =:= DNil]
  implicitly[i012#Div[i3003] =:= (i3#Neg *: i1 *: i2 *: i3#Neg *: DNil)]
  implicitly[DNil#Div[i012] =:= i012#Neg]
}