package scunits.integer

import scunits.types._

object Integer {
  implicitly[_0 + _0 =:= _0]  
  
  implicitly[p1 + p2 =:= p3]
  implicitly[p3 + n1 =:= p2]
  implicitly[p1 + n3 =:= n2]
  implicitly[p2 + n2 =:= _0]

  implicitly[n1 + n2 =:= n3]
  implicitly[n3 + p1 =:= n2]
  implicitly[n1 + p3 =:= p2]
  implicitly[n2 + p2 =:= _0]

  implicitly[_0 - _0 =:= _0]

  implicitly[p3 - p1 =:= p2]
  implicitly[p3 - n1 =:= p4]
  implicitly[p1 - p3 =:= n2]
  implicitly[p2 - p2 =:= _0]

  implicitly[n3 - n1 =:= n2]
  implicitly[n3 - p1 =:= n4]
  implicitly[n1 - n3 =:= p2]
  implicitly[n2 - n2 =:= _0]
}