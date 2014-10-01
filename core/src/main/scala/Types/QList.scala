package scunits.types

import scunits._

trait QList {
  type tail <: QList
  type append[R <: QList] <: QList
}
trait QNel extends QList {
  type head <: BaseQuantity
  type tail <: QList
  type Self = head :: tail
  type append[R <: QList] = head :: tail#append[R]
}
trait QNelOfHead[H <: BaseQuantity] extends QNel {
  type head = H
}
trait ::[L <: BaseQuantity,R <: QList] extends QNelOfHead[L] {
  type tail = R
}
trait QNil extends QList {
  type tail = QNil
  type append[R <: QList] = R
}