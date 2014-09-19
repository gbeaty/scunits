package scunits2

import scunits.types._

trait Quantity
trait BaseQuantity extends Quantity
trait DefaultQuantity extends BaseQuantity

object DefaultQuantities {
  trait Length extends DefaultQuantity
  trait Time extends DefaultQuantity
  trait Mass extends DefaultQuantity
  trait Temperature extends DefaultQuantity
  trait AmountOfSubstance extends DefaultQuantity
  trait Info extends DefaultQuantity
  trait Current extends DefaultQuantity
  trait Intensity extends DefaultQuantity

  type All = Length :: Time :: Mass :: Temperature :: AmountOfSubstance :: Current :: Intensity :: Info :: QNil
  type Dimless = All#Dimless
}

trait Dims {
  type Quants <: QList
  type Exps <: DList

  type Mult[R <: DimsOf[Quants]] = Quants ^ Exps#Mult[R#Exps]
  type Div[R <: DimsOf[Quants]] = Quants ^ Exps#Mult[R#Exps#Neg]

  type SetExp[I <: Integer,E <: Integer] = Quants ^ Exps#Set[I,E]

  protected type NegFunc[I <: Integer] = I#Neg
  type Neg = Quants ^ Exps#Neg
}
trait DimsOf[Q <: QList] extends Dims {
  type Quants = Q
}
class ^[L <: QList, R <: DList] extends DimsOf[L] {
  type Exps = R
}

trait DimsConverter[QI <: QList,I <: DList] {
  type QuantsIn = QI
  type Indexes = I
  type QuantsOut <: QList
  type Apply[EI <: DList] <: DList
}

package object default {
  type DimOf[I <: Integer] = DefaultQuantities.Dimless#Set[I,i1]
  type Length            = DimOf[i1]
  type Time              = DimOf[i2]
  type Mass              = DimOf[i3]
  type Temperature       = DimOf[i4]
  type AmountOfSubstance = DimOf[i5]
  type Current           = DimOf[i6]
  type Intensity         = DimOf[i7]
  type Info              = DimOf[i8]
}