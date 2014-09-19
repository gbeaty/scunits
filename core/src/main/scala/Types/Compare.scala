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

sealed trait Quantity
trait BaseQuantity extends Quantity
sealed trait DefaultQuantity extends BaseQuantity
trait QNil extends TNil[Quantity]

object Dims {
  trait Length extends DefaultQuantity
  trait Time extends DefaultQuantity
  trait Mass extends DefaultQuantity
  trait Temperature extends DefaultQuantity
  trait AmountOfSubstance extends DefaultQuantity
  trait Info extends DefaultQuantity
  trait Current extends DefaultQuantity
  trait Intensity extends DefaultQuantity

  trait Dims {
    type Quants <: TListOf[Quantity]
    type Exps <: TListOf[Integer]

    type Mult[R <: DimsOf[Quants]] = Quants ^ Exps#ZipMap[R#Exps,+]
    type Div[R <: DimsOf[Quants]] = Quants ^ Exps#ZipMap[R#Exps,-]
  }
  trait DimsOf[Q <: TListOf[Quantity]] extends Dims {
    type Quants = Q
  }
  trait ^[L <: TListOf[Quantity], R <: TListOf[Integer]] extends DimsOf[L] {
    type Exps = R
  }

  object DefaultQuantities extends 
    (Length :: Time :: Mass :: Temperature :: AmountOfSubstance :: Current :: Intensity :: Info :: QNil) {
    type ToZero[_] = _0
    type Dimless = MapTo[Integer,ToZero]
    trait Test[A]
    trait DimOf[I <: Integer] extends Test[Dimless#Set[I,_1]]
    // type DimOf[I <: Integer] = Dimless#Set[I,_1]
    // type TT = Dimless#Set[_1,_1]

    /*type Length = DimOf[_0]
    type Time = DimOf[_1]
    type Mass = DimOf[_2]
    type Temperature = DimOf[_3]
    type AmountOfSubstance = DimOf[_4]
    type Current = DimOf[_5]
    type Intensity = DimOf[_6]
    type Info = DimOf[_7]*/
    // type Length = Dimless#Set[_0,_1]
    // val a: Dimless#Set[_0,_1] = 1
  }
}

/*trait Quantity
trait BaseQuantity extends Quantity
trait SiBaseQuantity extends Quantity
trait AbstractQuantity extends Quantity

trait QuantList extends TListOf[BaseQuantity]
trait ExpList extends TListOf[Integer]

trait Dimms {
  type Quant <: TListOf[BaseQuantity]
  type Exp <: TListOf[Integer]

  type Mult[R <: DimsOf[Quant]] = Quant ^ Exp#ZipMap[R#Exp,+]
  type Div[R <: DimsOf[Quant]] = Quant ^ Exp#ZipMap[R#Exp,-]
}
trait DimsOf[Q <: TListOf[BaseQuantity]] extends Dimms {
  type Quant = Q
}
trait ^[L <: TListOf[BaseQuantity], R <: TListOf[Integer]] extends DimsOf[L] {
  type Exp = R
}*/

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

ABCD
AB  EF
*/

/*
Quant[Index]
scunits = A=1
libA    = A=1,B=2
libB    = A=1,C=2

Index[Quant]
scunits = 1=A
libA    = 1=A,2=B
libB    = 1=A,2=C
*/