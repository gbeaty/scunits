package scunits.types

import scunits._
import TestTypes._

object DListTests {
  // Test head:
  implicitly[ENil#head =:= _0]
  implicitly[i3003#head =:= p3]

  // Test neg:
  implicitly[ENil#neg =:= ENil]
  implicitly[i012#neg =:= (_0 *: n1 *: n2 *: ENil)]

  // Test set:
  implicitly[ENil#set[_0,p1] =:= (p1 *: ENil)]
  implicitly[ENil#set[p1,p3] =:= (_0 *: p3 *: ENil)]
  implicitly[ENil#set[p2,p1] =:= (_0 *: _0 *: p1 *: ENil)]
  implicitly[ENil#set[p4,p4] =:= (_0 *: _0 *: _0 *: _0 *: p4 *: ENil)]

  // Test mult:
  implicitly[i11#mult[ENil] =:= i11]
  implicitly[ENil#mult[i11] =:= i11]
  implicitly[ENil#mult[ENil] =:= ENil]
  implicitly[i012#mult[i3003] =:= (p3 *: p1 *: p2 *: p3 *: ENil)]
  implicitly[i3003#neg#mult[i3003] =:= ENil]

  // Test div:
  implicitly[i012#div[ENil] =:= i012]
  implicitly[ENil#div[ENil] =:= ENil]
  implicitly[i012#div[i012] =:= ENil]
  implicitly[i012#div[i3003] =:= (n3 *: p1 *: p2 *: n3 *: ENil)]
  implicitly[ENil#div[i012] =:= i012#neg]

  // Test zero truncating:
  implicitly[i012#div[i102] =:= (n1 *: p1 *: ENil)]
  implicitly[i2113#div[i1103] =:= i101]

  // Test zeros:
  implicitly[EList#zeros[p1,p2] =:= (_0 *: p2 *: ENil)]
  implicitly[EList#zeros[p3,p2] =:= (_0 *: _0 *: _0 *: p2 *: ENil)]
}