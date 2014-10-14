package scunits.test

import scunits._
import scunits.integer._
import scunits.integer.Ops._

object Integer {
  implicitly[_0 + _0 =:= _0]
  implicitly[_0 - _0 =:= _0]
  implicitly[p1 + p2 =:= p3]
  implicitly[p1 - p2 =:= n1]
  implicitly[n1 + p2 =:= p1]
  implicitly[n1 + n2 =:= n3]
}