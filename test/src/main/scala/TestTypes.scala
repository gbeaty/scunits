package scunits.types

import scunits._

object TestTypes {
  object A extends BaseQuantity
  type A = A.type
  object B extends BaseQuantity
  type B = B.type
  object C extends BaseQuantity
  type C = C.type
  object D extends BaseQuantity
  type D = D.type
  object E extends BaseQuantity
  type E = E.type
  object Z extends BaseQuantity
  type Z = Z.type

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
  type BCDn = ABCDn#Tail
  type CDn = BCDn#Tail
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

  trait Nilq extends Quantities {
    type Quants = QNil
  }
  object Nilq extends Nilq

  trait ABq extends Quantities {
    type Quants = (A.type :: B.type :: QNil)
    type A = DimOf[i0]
    type B = DimOf[i1]
  }
  object ABq extends ABq

  trait ABCq extends Quantities {
    type Quants = (A.type :: B.type :: C.type :: QNil)
    type A = DimOf[i0]
    type B = DimOf[i1]
    type C = DimOf[i2]
  }
  object ABCq extends ABCq

  trait DEq extends Quantities {
    type Quants = (D.type :: E.type :: QNil)
    type D = DimOf[i0]
    type E = DimOf[i1]
  }
  object DEq extends DEq
}