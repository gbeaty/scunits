package scunits.types

import scunits._
import TestTypes._

object QListTests {
  implicitly[ABCDn#Merge[ABn] =:= ABCDn]
  implicitly[ABn#Merge[ABCDn] =:= ABCDn]
  implicitly[QNil#Merge[ABCDn] =:= ABCDn]
  implicitly[ABCDn#Merge[QNil] =:= ABCDn]

  implicitly[QNil#append[ABn] =:= ABn]
  implicitly[ABn#append[QNil] =:= ABn]
  implicitly[ABn#append[CDn] =:= ABCDn]
  implicitly[Bn#append[An] =:= (B :: A :: QNil)]
}