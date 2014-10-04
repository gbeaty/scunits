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

  object QNil extends Quantities {
    type quants = QNil
  }

  type ABn = A :: B :: QNil
  object ABq extends Quantities {
    type quants = ABn
    type A = dimOf[_0]
    type B = dimOf[p1]
  }
  type ABq = ABq.type

  type ABCn = A :: B :: C :: QNil
  object ABCq extends Quantities {
    type quants = ABCn
    type A = dimOf[_0]
    type B = dimOf[p1]
    type C = dimOf[p2]
  }
  type ABCq = ABCq.type

  type ABDn = A :: B :: D :: QNil
  object ABDq extends Quantities {
    type quants = ABDn
    type A = dimOf[_0]
    type B = dimOf[p1]
    type D = dimOf[p2]
  }
  type ABDq = ABDq.type

  type ABCDn = A :: B :: C :: D :: QNil
  object ABCDq extends Quantities {
    type quants = ABCDn
    type A = dimOf[_0]
    type B = dimOf[p1]
    type C = dimOf[p2]
    type D = dimOf[p3]
  }
  type ABCDq = ABCDq.type

  type An = A :: QNil
  object Aq extends Quantities {
    type quants = An
    type A = dimOf[_0]
  }
  type Aq = Aq.type

  type Bn = B :: QNil
  object Bq extends Quantities {
    type quants = Bn
    type B = dimOf[_0]
  }
  type Bq = Bn

  type BCn = B :: C :: QNil
  object BCq extends Quantities {
    type quants = BCn
    type B = dimOf[_0]
    type C = dimOf[p1]
  }
  type BCq = BCn

  type Cn = C :: QNil
  object Cq extends Quantities {
    type quants = Cn
    type C = dimOf[_0]
  }
  type Cq = Cn

  type CDn = C :: D :: QNil
  object CDq extends Quantities {
    type quants = CDn
    type C = dimOf[_0]
    type D = dimOf[p1]
  }
  type CDq = CDn

  type Zn = Z :: QNil
  object Zq extends Quantities {
    type quants = Zn
    type Z = dimOf[_0]
  }
  type Zq = Zq.type

  type i00 = _0 *: _0 *: ENil
  type i11 = p1 *: p1 *: ENil
  type i111 = p1 *: i11
  type i12 = p1 *: p2 *: ENil
  type i21 = p2 *: p1 *: ENil
  type i012 = _0 *: p1 *: p2 *: ENil
  type i102 = p1 *: _0 *: p2 *: ENil
  type i0012 = _0 *: i012
  type i3003 = p3 *: _0 *: _0 *: p3 *: ENil
  type i1234 = p1 *: p2 *: p3 *: p4 *: ENil
  type i4321 = p4 *: p3 *: p2 *: p1 *: ENil

  type Rev4 = p3 -: p2 -: p1 -: _0 -: INil
  type Rev2 = p1 -: _0 -: INil
  type Skip1 = p1 -: INil
}