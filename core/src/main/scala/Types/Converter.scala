package scunits.types

import scunits._

class Converter {
  type from <: QList
  type to <: QList  

  type exps[FE <: EList] <: EList
  type apply[D <: Dims] = to ^ exps[D#exps]
  def apply[D <: DimsOf[from]](in: Measure[D]) = Measure[apply[D]](in.v)
}
class ConverterFrom[F <: QList] extends Converter {
  type from = F
}
@annotation.implicitNotFound(msg = "Cannot find a Converter of type ${F} -> ${T}.")
class ConverterFromTo[F <: QList, T <: QList] extends ConverterFrom[F] {
  type to = T
}
class IndicesConverter[F <: QList, T <: QList, I <: IList] extends ConverterFromTo[F,T] {
  type indices = I
  type exps[FE <: EList] = indices#convertDims[FE]
}
class IdentityConverter[Qs <: QList] extends ConverterFromTo[Qs,Qs] {
  type exps[FE <: EList] = FE
}

class SearchResult {
  type at <: Integer
}
class Found[I <: Integer] extends SearchResult {
  type at = I
}
class NotFound extends SearchResult {
  type at = n1
}
class QuantSearched[Qs <: QList, Q <: BaseQuantity, I <: Integer] {
  type res <: SearchResult
}
class QuantSearch[Qs <: QList, Q <: BaseQuantity, I <: Integer, R <: SearchResult] extends QuantSearched[Qs,Q,I] {
  type res = R
}

class DimensionOf[Qs <: QList, B <: BaseQuantity, E <: EList] {
  type exps = E
  type dims = Qs ^ E
}

trait CachedConverter

/*
AB
 BC

AB => BC = - 0
BC => AB = 1 -

BC[1,0] = AB[0,1]

AB[0,1] * BC[1,0]
*/