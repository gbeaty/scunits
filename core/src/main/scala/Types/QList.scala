package scunits.types

import scunits._

trait QList {
  type Tail <: QList
  type Size <: NonNegInt
  type MapHeadOrElse[F[_ <: Quantity] <: QList, Else <: QList] <: QList
  type Merge[R <: QList] <: QList
  protected type MergeNel[L <: QNel] <: QList
}
trait QNel extends QList {
  type Head <: Quantity
  type Tail <: QList
  type Self = Head :: Tail
  type Size = Tail#Size#succ

  type MapHeadOrElse[F[_ <: Quantity] <: QList, Else <: QList] = F[Head]

  type Merge[R <: QList] = R#MergeNel[Self]
  protected type MergeNel[L <: QNel] = Head :: L#Tail#Merge[Tail]
}
trait QNelOfHead[H <: Quantity] extends QNel {
  type Head = H
}
trait ::[L <: Quantity,R <: QList] extends QNelOfHead[L] {
  type Tail = R
}
trait QNil extends QList {
  type Tail = QNil
  type Size = i0
  type MapHeadOrElse[F[_ <: Quantity] <: QList, Else <: QList] = Else
  type Merge[R <: QList] = R
  protected type MergeNel[L <: QNel] = L
}