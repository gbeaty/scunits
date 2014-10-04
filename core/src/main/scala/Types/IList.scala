package scunits.types

import scunits._

trait IList {
  type convertDims[E <: EList] = convertingDims[E,ENil]#truncZeros
  protected type convertingDims[E <: EList, Res <: EList] <: EList
}
trait INel extends IList {
  type head <: IntBox
  type tail <: IList
}
trait =:[L <: IntBox, R <: IList] extends INel {
  type head = L
  type tail = R

  protected type convertingDims[E <: EList, Res <: EList] =
    tail#convertingDims[
      E#tail,
      // head#isNeg#branch[EList, Res, Res#set[head,E#head]]
      head#doOrElse[EList, ({type S[H <: Integer] = Res#set[H,E#head]})#S, Res]
    ]
}

trait INil extends IList {
  protected type convertingDims[E <: EList, Res <: EList] = Res
}