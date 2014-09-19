package scunits2

import scunits.types._

sealed trait Quantity
trait BaseQuantity extends Quantity
sealed trait DefaultQuantity extends BaseQuantity
trait QNil extends TNil[Quantity]

object DefaultQuantities {
  trait Length extends DefaultQuantity
  trait Time extends DefaultQuantity
  trait Mass extends DefaultQuantity
  trait Temperature extends DefaultQuantity
  trait AmountOfSubstance extends DefaultQuantity
  trait Info extends DefaultQuantity
  trait Current extends DefaultQuantity
  trait Intensity extends DefaultQuantity

  type ToZero[_] = _0

  val all = new (Length :: Time :: Mass :: Temperature :: AmountOfSubstance :: Current :: Intensity :: Info :: QNil)
  val dimless = new (all.This ^ all.MapTo[Integer,ToZero])
}

trait Dims {
  type Quants <: TListOf[Quantity]
  type Exps <: TListOf[Integer]

  type Mult[R <: DimsOf[Quants]] = Quants ^ Exps#ZipMap[R#Exps,+]
  type Div[R <: DimsOf[Quants]] = Quants ^ Exps#ZipMap[R#Exps,-]

  type SetExp[I <: Integer,E <: Integer] = Quants ^ Exps#Set[I,E]

  protected type NegFunc[I <: Integer] = I#Neg
  type Neg = Quants ^ Exps#Map[NegFunc]
}
trait DimsOf[Q <: TListOf[Quantity]] extends Dims {
  type Quants = Q
}
class ^[L <: TListOf[Quantity], R <: TListOf[Integer]] extends DimsOf[L] {
  type Exps = R
}

package object default {  
  
  type DimOf[I <: Integer] = DefaultQuantities.dimless.SetExp[I,_1]
  type Length            = DimOf[_1]
  type Time              = DimOf[_2]
  type Mass              = DimOf[_3]
  type Temperature       = DimOf[_4]
  type AmountOfSubstance = DimOf[_5]
  type Current           = DimOf[_6]
  type Intensity         = DimOf[_7]
  type Info              = DimOf[_8]
}