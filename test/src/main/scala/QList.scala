package scunits.types

import scunits._

object TestTypes {
  trait A extends BaseQuantity
  trait B extends BaseQuantity
  trait C extends BaseQuantity
  trait D extends BaseQuantity
  trait Z extends BaseQuantity

  type ABn = A :: B :: QNil
  object ABn extends Quantities {
    type Quants = ABn
  }
  type ABCn = A :: B :: C :: QNil
  object ABCn extends Quantities {
    type Quants = ABCn
  }
  type ABDn = A :: B :: D :: QNil
  object ABDn extends Quantities {
    type Quants = ABDn
  }
  type ABCDn = A :: B :: C :: D :: QNil
  object ABCDn extends Quantities {
    type Quants = ABCDn
  }
  type Zn = Z :: QNil
  object Zn extends Quantities {
    type Quants = Zn
  }

  type i00 = i0 *: i0 *: DNil
  type i11 = i1 *: i1 *: DNil
  type i111 = i1 *: i11
  type i21 = i2 *: i1 *: DNil
  type i012 = i0 *: i1 *: i2 *: DNil
  type i102 = i1 *: i0 *: i2 *: DNil
  type i0012 = i0 *: i012
  type i3003 = i3 *: i0 *: i0 *: i3 *: DNil
  type i1234 = i1 *: i2 *: i3 *: i4 *: DNil
  type i4321 = i4 *: i3 *: i2 *: i1 *: DNil

  type Rev4 = i3 -: i2 -: i1 -: i0 -: INil
  type Rev2 = i1 -: i0 -: INil
}
import TestTypes._

object QListTests {
  implicitly[ABCDn#Merge[ABn] =:= ABCDn]
  implicitly[ABn#Merge[ABCDn] =:= ABCDn]
  implicitly[QNil#Merge[ABCDn] =:= ABCDn]
  implicitly[ABCDn#Merge[QNil] =:= ABCDn]

  // type AppendTo[A <: QList, B <: QList with A#Merge[B]] = True
  // type TestGood = AppendTo[ABCDn, ABn]
  // type TestBad = AppendTo[ABCDn, ABDn]
}