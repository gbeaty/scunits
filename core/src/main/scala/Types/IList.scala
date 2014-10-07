package scunits.types

import scunits._

trait IList {
  type apply[E <: EList] = convertingDims2[E,ENil]#map[({type t[E <: EList] = E#truncZeros})#t]
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

  protected type convertingDims2[E <: EList, Res <: EList] = ({
    type recurse[R <: EList] = tail#convertingDims2[E#tail, R]
    type set[H <: Integer] = Res#set[H,E#head]
    type apply = E#head#isZero#branch[
      Box[EList],
      recurse[Res], // E#head == _0, do nothing.
      head#mapTo[EList, set]#flatMap[recurse]] // E#head != _0, convert if possible, abort if not.
  })#apply

  protected type convertingDims[E <: EList, Res <: EList] =
    tail#convertingDims[
      E#tail,
      head#mapTo[EList, ({type S[H <: Integer] = Res#set[H,E#head]})#S]#getOrElse[Res]
    ]
}

trait INil extends IList {
  protected type convertingDims[E <: EList, Res <: EList] = Res
  protected type convertingDims2[E <: EList, Res <: EList] = Full[EList, Res]
}