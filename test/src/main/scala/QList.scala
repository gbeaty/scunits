package scunits.types

import scunits._
import TestTypes._

object QListTests {
  implicitly[QNil#append[ABn] =:= ABn]
  implicitly[ABn#append[QNil] =:= ABn]
  implicitly[ABn#append[CDn] =:= ABCDn]
  implicitly[Bn#append[An] =:= (B :: A :: QNil)]
}