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
  object ABq extends Quantities {
    type Quants = ABn
    type A = DimOf[i0]
    type B = DimOf[i1]
  }
  type ABq = ABq.type

  type ABCn = A :: B :: C :: QNil
  object ABCq extends Quantities {
    type Quants = ABCn
    type A = DimOf[i0]
    type B = DimOf[i1]
    type C = DimOf[i2]
  }
  type ABCq = ABCq.type

  type ABDn = A :: B :: D :: QNil
  object ABDq extends Quantities {
    type Quants = ABDn
    type A = DimOf[i0]
    type B = DimOf[i1]
    type D = DimOf[i2]
  }
  type ABDq = ABDq.type

  type ABCDn = A :: B :: C :: D :: QNil
  object ABCDq extends Quantities {
    type Quants = ABCDn
    type A = DimOf[i0]
    type B = DimOf[i1]
    type C = DimOf[i2]
    type D = DimOf[i3]
  }
  type ABCDq = ABCDq.type

  type An = A :: QNil
  object Aq extends Quantities {
    type Quants = An
    type A = DimOf[i0]
  }
  type Aq = Aq.type

  type Bn = B :: QNil
  object Bq extends Quantities {
    type Quants = Bn
    type B = DimOf[i0]
  }
  type Bq = Bn

  type BCn = B :: C :: QNil
  object BCq extends Quantities {
    type Quants = BCn
    type B = DimOf[i0]
    type C = DimOf[i1]
  }
  type BCq = BCn

  type Cn = C :: QNil
  object Cq extends Quantities {
    type Quants = Cn
    type C = DimOf[i0]
  }
  type Cq = Cn

  type Zn = Z :: QNil
  object Zq extends Quantities {
    type Quants = Zn
    type Z = DimOf[i0]
  }
  type Zq = Zq.type

  type i00 = i0 *: i0 *: DNil
  type i11 = i1 *: i1 *: DNil
  type i111 = i1 *: i11
  type i12 = i1 *: i2 *: DNil
  type i21 = i2 *: i1 *: DNil
  type i012 = i0 *: i1 *: i2 *: DNil
  type i102 = i1 *: i0 *: i2 *: DNil
  type i0012 = i0 *: i012
  type i3003 = i3 *: i0 *: i0 *: i3 *: DNil
  type i1234 = i1 *: i2 *: i3 *: i4 *: DNil
  type i4321 = i4 *: i3 *: i2 *: i1 *: DNil

  type Rev4 = i3 -: i2 -: i1 -: i0 -: INil
  type Rev2 = i1 -: i0 -: INil
  type Skip1 = i1 -: INil
}