package scunits.types

import scunits._

trait Indexes {
  type Convert[D <: Dims] = ConvertQuants[D#Quants] ^ ConvertDims[D#Exps]
  type ConvertQuants[Q <: QList] = ConvertingQuants[Q,Q]
  type ConvertDims[E <: DList] = ConvertingDims[E,E]#TruncZeros
  protected type ConvertingQuants[Q <: QList, Res <: QList] <: QList
  protected type ConvertingDims[E <: DList, Res <: DList] <: DList
}
trait IndexNel extends Indexes {
  type Head <: NonNegInt
  type Tail <: Indexes
}
trait -:[L <: NonNegInt, R <: Indexes] extends IndexNel {
  type Head = L
  type Tail = R

  // protected type ConvertingQuants[Q <: QList, Res <: QList] = Tail#ConvertingQuants[Q#Tail, Q#MapHeadOrElse[({type R[H <: Quantity] = Res#Set[Head,H]})#R, Res]]
  protected type ConvertingDims[E <: DList, Res <: DList] = Tail#ConvertingDims[E#Tail, Res#Set[Head,E#Head]]  
}
trait INil extends Indexes {
  protected type ConvertingQuants[Q <: QList, Res <: QList] = Res
  protected type ConvertingDims[E <: DList, Res <: DList] = Res
}