package scunits.types

import scunits._

trait QList {
  type Merge[R <: QList] <: QList
  protected type MergeNel[L <: QNel] <: QList
}
trait QNel extends QList {
  type Head <: Quantity
  type Tail <: QList
  type Self = Head :: Tail

  type Merge[R <: QList] = R#MergeNel[Self]
  protected type MergeNel[L <: QNel] = Head :: L#Tail#Merge[Tail]
}
trait ::[L <: Quantity,R <: QList] extends QNel {
  type Head = L
  type Tail = R
}
trait QNil extends QList {
  type Merge[R <: QList] = R
  protected type MergeNel[L <: QNel] = L
}