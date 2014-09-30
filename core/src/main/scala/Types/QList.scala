package scunits.types

import scunits._

trait QList {
  type Tail <: QList
  type Size <: NonNegInt
  type MapHeadOrElse[F[_ <: BaseQuantity] <: QList, Else <: QList] <: QList
  type Merge[R <: QList] <: QList
  type append[R <: QList] <: QList
  protected type MergeNel[L <: QNel] <: QList
}
trait QNel extends QList {
  type Head <: BaseQuantity
  type Tail <: QList
  type Self = Head :: Tail
  type Size = Tail#Size#succ

  type MapHeadOrElse[F[_ <: BaseQuantity] <: QList, Else <: QList] = F[Head]

  type Merge[R <: QList] = R#MergeNel[Self]
  protected type MergeNel[L <: QNel] = Head :: L#Tail#Merge[Tail]

  type append[R <: QList] = Head :: Tail#append[R]
}
trait QNelOfHead[H <: BaseQuantity] extends QNel {
  type Head = H
}
trait ::[L <: BaseQuantity,R <: QList] extends QNelOfHead[L] {
  type Tail = R
}
trait QNil extends QList {
  type Tail = QNil
  type Size = i0
  type MapHeadOrElse[F[_ <: BaseQuantity] <: QList, Else <: QList] = Else
  type Merge[R <: QList] = R
  protected type MergeNel[L <: QNel] = L
  type append[R <: QList] = R
}