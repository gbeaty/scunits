package scunits.types

import scunits._

trait QList {
  type Dimless <: DList  
}
trait QNel extends QList {
  type Head <: Quantity
  type Tail <: QList

  type Dimless = i0 *: Tail#Dimless

}
trait ::[L <: Quantity,R <: QList] extends QNel {
  type Head = L
  type Tail = R
}
trait QNil extends QList {
  type Dimless = DNil
}