package scunits.types

import scunits._

trait DimsOf[Qs <: QList] extends Dims {
  type qlist = Qs
  override type values <: Qs#base
}
trait DimsConst[Qs <: QList, Vs <: Qs#base] extends DimsOf[Qs] {
  type values = Vs
}

trait BaseQuantity {
  type of <: Dim
  type set[E <: Integer] <: of
  type get[L <: of] <: Integer
}