package scunits.types

import TestTypes._

object IndexesTests {
  implicitly[Rev4#ConvertDims[i3003] =:= i3003]
  implicitly[Rev2#ConvertDims[i012] =:= i102]
  implicitly[Rev4#ConvertDims[i0012] =:= i21]
  implicitly[Rev4#ConvertDims[i1234] =:= i4321]
  implicitly[Rev4#ConvertDims[i4321] =:= i1234]
}