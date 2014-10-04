package scunits.types

import TestTypes._

object IndexesTests {
  // ILists must have the same length as DLists (this is not enforced).
  implicitly[Rev4#convertDims[i3003] =:= i3003]
  implicitly[Rev2#convertDims[i21] =:= i12]
  implicitly[Rev4#convertDims[i0012] =:= i21]
  implicitly[Rev4#convertDims[i1234] =:= i4321]
  implicitly[Rev4#convertDims[i4321] =:= i1234]
  implicitly[Skip1#convertDims[p1 *: ENil] =:= (_0 *: p1 *: ENil)]
}