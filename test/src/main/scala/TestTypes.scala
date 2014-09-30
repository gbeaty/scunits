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
    type quants = ABn
    type A = dimOf[i0]
    type B = dimOf[i1]
  }
  type ABq = ABq.type

  type ABCn = A :: B :: C :: QNil
  object ABCq extends Quantities {
    type quants = ABCn
    type A = dimOf[i0]
    type B = dimOf[i1]
    type C = dimOf[i2]
  }
  type ABCq = ABCq.type

  type ABDn = A :: B :: D :: QNil
  object ABDq extends Quantities {
    type quants = ABDn
    type A = dimOf[i0]
    type B = dimOf[i1]
    type D = dimOf[i2]
  }
  type ABDq = ABDq.type

  type ABCDn = A :: B :: C :: D :: QNil
  object ABCDq extends Quantities {
    type quants = ABCDn
    type A = dimOf[i0]
    type B = dimOf[i1]
    type C = dimOf[i2]
    type D = dimOf[i3]
  }
  type ABCDq = ABCDq.type

  type An = A :: QNil
  object Aq extends Quantities {
    type quants = An
    type A = dimOf[i0]
  }
  type Aq = Aq.type

  type Bn = B :: QNil
  object Bq extends Quantities {
    type quants = Bn
    type B = dimOf[i0]
  }
  type Bq = Bn

  type BCn = B :: C :: QNil
  object BCq extends Quantities {
    type quants = BCn
    type B = dimOf[i0]
    type C = dimOf[i1]
  }
  type BCq = BCn

  type Cn = C :: QNil
  object Cq extends Quantities {
    type quants = Cn
    type C = dimOf[i0]
  }
  type Cq = Cn

  type CDn = C :: D :: QNil
  object CDq extends Quantities {
    type quants = CDn
    type C = dimOf[i0]
    type D = dimOf[i1]
  }
  type CDq = CDn

  type Zn = Z :: QNil
  object Zq extends Quantities {
    type quants = Zn
    type Z = dimOf[i0]
  }
  type Zq = Zq.type

  type i00 = i0 *: i0 *: ENil
  type i11 = i1 *: i1 *: ENil
  type i111 = i1 *: i11
  type i12 = i1 *: i2 *: ENil
  type i21 = i2 *: i1 *: ENil
  type i012 = i0 *: i1 *: i2 *: ENil
  type i102 = i1 *: i0 *: i2 *: ENil
  type i0012 = i0 *: i012
  type i3003 = i3 *: i0 *: i0 *: i3 *: ENil
  type i1234 = i1 *: i2 *: i3 *: i4 *: ENil
  type i4321 = i4 *: i3 *: i2 *: i1 *: ENil

  type Rev4 = i3 -: i2 -: i1 -: i0 -: INil
  type Rev2 = i1 -: i0 -: INil
  type Skip1 = i1 -: INil
}