package scunits.types

import TestTypes._

object IndexesTests {
  // Full conversions:
  // implicitly[Rev4#apply[i3003] =:= i3003]
  // implicitly[Rev2#apply[i21] =:= i12]
  // implicitly[Rev4#apply[i0012] =:= i21]
  // implicitly[Rev4#apply[i1234] =:= i4321]
  // implicitly[Rev4#apply[i4321] =:= i1234]
  // implicitly[Skip1#apply[p1 *: ENil] =:= (_0 *: p1 *: ENil)]

  // Partials:
  implicitly[c23To01#apply[i0012] =:= Full[EList, i12]]
  implicitly[c23To01#apply[i21] =:= Empty[EList]]
  implicitly[c23To01#apply[i3003] =:= Empty[EList]]
}