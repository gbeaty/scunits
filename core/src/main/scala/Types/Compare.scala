package scunits.types

trait Compared {
  type IsLess <: Bool
  type IsEqual <: Bool
  type IsGreater <: Bool

  type IsLessOrEqual = IsLess || IsEqual
  type IsGreaterOrEqual = IsGreater || IsEqual
}
trait EqualOrLess extends Compared
trait EqualOrGreater extends Compared
trait NotEqual extends Compared
trait Less extends EqualOrLess with NotEqual {
  type IsLess = True
  type IsEqual = False
  type IsGreater = False
}
trait Equal extends EqualOrLess with EqualOrGreater {
  type IsLess = False
  type IsEqual = True
  type IsGreater = False
}
trait Greater extends EqualOrGreater with NotEqual {
  type IsLess = False
  type IsEqual = False
  type IsGreater = True
}

trait Comparable {
  type With <: ComparableWith[With]
  type Compare[R <: With] <: Compared
}
trait ComparableWith[W <: ComparableWith[W]] extends Comparable {
  type With = W  
}

sealed trait Quantity {
  type Id <: Integer
  type CompareAbstract[R <: AbstractQuantity] <: Compared
  type CompareSi[R <: SiBaseQuantity] <: Compared
  type CompareNonSi[R <: NonSiBaseQuantity] <: Compared
}
sealed trait AbstractQuantity extends Quantity
sealed trait AbstractQuantityOf[I <: Integer] extends AbstractQuantity {
  type Id = I
  type Compare[R <: Quantity] = R#CompareAbstract[AbstractQuantityOf[I]]
  type CompareAbstract[L <: AbstractQuantity] = L#Id#Compare[Id]
  type CompareSi[L <: BaseQuantity] = Greater
  type CompareNonSi[R <: NonSiBaseQuantity] = Greater
}
sealed trait BaseQuantity extends Quantity {
  type CompareAbstract[L <: AbstractQuantity] = Less
}
sealed trait SiBaseQuantity extends BaseQuantity
sealed trait SiBaseQuantityOf[I <: Integer] extends SiBaseQuantity {
  type Id = I
  type Compare[R <: Quantity] = R#CompareSi[SiBaseQuantityOf[I]]  
  type CompareSi[L <: SiBaseQuantity] = L#Id#Compare[Id]
}
trait NonSiBaseQuantity extends BaseQuantity
trait NonSiBaseQuantityOf[I <: Integer] extends BaseQuantity {
  type Id = I
  type CompareSi[L <: SiBaseQuantity] = Less
}

/*
scunits = AB
libA    = ABCD, with order = ABCD
libB    = ABEF, with order = ABEF
AB   * C    = ABC,    uses  SiOrder
AB   * D    = ABD,    uses  SiOrder
AB   * CD   = ABCD,   uses  Order[ABCD]
ABC  * ABE  = ABCE,   needs Order[CE]
ABCE * ABDF = ABCDEF, needs Order[CDEF]

ABCE * ABDF = Ordered[A^2 B^2] Unordered[C E D F]
*/