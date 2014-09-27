package scunits.types

import scunits._

trait IList {
  type ConvertDims[E <: EList] = ConvertingDims[E,ENil]#TruncZeros
  protected type ConvertingDims[E <: EList, Res <: EList] <: EList
}
trait INel extends IList {
  type Head <: Integer
  type Tail <: IList
}
trait -:[L <: Integer, R <: IList] extends INel {
  type Head = L
  type Tail = R

  protected type ConvertingDims[E <: EList, Res <: EList] = Tail#ConvertingDims[E#Tail, Res#Set[Head,E#Head]]  
}
trait INil extends IList {
  protected type ConvertingDims[E <: EList, Res <: EList] = Res
}