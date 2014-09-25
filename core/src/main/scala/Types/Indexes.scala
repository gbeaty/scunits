package scunits.types

import scunits._

trait Indexes {
  type ConvertDims[E <: DList] = ConvertingDims[E,E]#TruncZeros
  protected type ConvertingDims[E <: DList, Res <: DList] <: DList
}
trait IndexNel extends Indexes {
  type Head <: Integer
  type Tail <: Indexes
}
trait -:[L <: Integer, R <: Indexes] extends IndexNel {
  type Head = L
  type Tail = R

  protected type ConvertingDims[E <: DList, Res <: DList] = Tail#ConvertingDims[E#Tail, Res#Set[Head,E#Head]]  
}
trait INil extends Indexes {
  protected type ConvertingDims[E <: DList, Res <: DList] = Res
}