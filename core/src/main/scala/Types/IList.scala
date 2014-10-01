package scunits.types

import scunits._

trait IList {
  type convertDims[E <: EList] = convertingDims[E,ENil]#truncZeros
  protected type convertingDims[E <: EList, Res <: EList] <: EList
}
trait INel extends IList {
  type head <: Integer
  type tail <: IList
}
trait -:[L <: Integer, R <: IList] extends INel {
  type head = L
  type tail = R

  protected type convertingDims[E <: EList, Res <: EList] = tail#convertingDims[E#tail, Res#set[head,E#head]]  
}
trait INil extends IList {
  protected type convertingDims[E <: EList, Res <: EList] = Res
}