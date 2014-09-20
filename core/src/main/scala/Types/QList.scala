package scunits.types

trait QList {
  type Dimless <: DList  
}
trait QNel extends QList {
  type Head <: scunits2.Quantity
  type Tail <: QList

  type Dimless = i0 *: Tail#Dimless

}
trait ::[L <: scunits2.Quantity,R <: QList] extends QNel {
  type Head = L
  type Tail = R
}
trait QNil extends QList {
  type Dimless = DNil
}