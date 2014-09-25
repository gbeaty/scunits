package scunits.types

import scunits._

trait IList {
  type ConvertDims[E <: DList] = ConvertingDims[E,DNil]#TruncZeros
  protected type ConvertingDims[E <: DList, Res <: DList] <: DList
}
trait INel extends IList {
  type Head <: Integer
  type Tail <: IList
}
trait -:[L <: Integer, R <: IList] extends INel {
  type Head = L
  type Tail = R

  protected type ConvertingDims[E <: DList, Res <: DList] = Tail#ConvertingDims[E#Tail, Res#Set[Head,E#Head]]  
}
trait INil extends IList {
  protected type ConvertingDims[E <: DList, Res <: DList] = Res
}