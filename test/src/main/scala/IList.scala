package scunits.types

import TestTypes._

object IndexesTests {
  // ILists must have the same length as DLists (this is not enforced).
  implicitly[Rev4#ConvertDims[i3003] =:= i3003]
  implicitly[Rev2#ConvertDims[i21] =:= i12]
  implicitly[Rev4#ConvertDims[i0012] =:= i21]
  implicitly[Rev4#ConvertDims[i1234] =:= i4321]
  implicitly[Rev4#ConvertDims[i4321] =:= i1234]
  implicitly[Skip1#ConvertDims[i1 *: DNil] =:= (i0 *: i1 *: DNil)]
}