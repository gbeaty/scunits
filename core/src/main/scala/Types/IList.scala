package scunits.types

import scunits._

trait IList {
  type convertDims2[E <: EList] = convertingDims2[E,ENil]#map[({type t[E <: EList] = E#truncZeros})#t]
  protected type convertingDims2[E <: EList, Res <: EList] <: Box[EList]

  type convertDims[E <: EList] = convertingDims[E,ENil]#truncZeros
  protected type convertingDims[E <: EList, Res <: EList] <: EList
}
trait INel extends IList {
  type head <: Box[Integer]
  type tail <: IList
}
trait =:[L <: Box[Integer], R <: IList] extends INel {
  type head = L
  type tail = R

  /*protected type convertingDims[E <: EList, Res <: EList] = {
    type recurse[S <: EList] = tail#convertingDims[E#tail, S]
    type set[H <: Integer] = Res#set[H,E#head]
    type apply = head#getOrElse[
      ???,
      recurse[set], // Defined index for this exponent.
      E#isZero#not#branch[INVALID, INVALID, recurse[Res]]
    ]
  }#apply*/
  protected type convertingDims[E <: EList, Res <: EList] =
    tail#convertingDims[
      E#tail,
      head#getOrElse[EList, ({type S[H <: Integer] = Res#set[H,E#head]})#S, Res]
    ]
}

trait INil extends IList {
  protected type convertingDims[E <: EList, Res <: EList] = Res
}