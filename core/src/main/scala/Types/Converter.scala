package scunits.types

import scunits._
import annotation.implicitNotFound

class Converter {
  type from <: QList
  type to <: QList  

  type exps[FE <: EList] <: Box[EList]
  def apply[F <: DimsOf[from], R <: EList](m: Scalar[F])(implicit c: ConvertResult[exps[F#exps],R]) =
    Scalar[to ^ R](m.v)
}
class ConverterFrom[F <: QList] extends Converter {
  type from = F
}
@implicitNotFound(msg = "Cannot find a Converter of type ${F} -> ${T}.")
class ConverterFromTo[F <: QList, T <: QList] extends ConverterFrom[F] {
  type to = T
}
class IndicesConverter[F <: QList, T <: QList, I <: IList] extends ConverterFromTo[F,T] {
  type indices = I
  type exps[FE <: EList] = indices#apply[FE]
}
class IdentityConverter[Qs <: QList] extends ConverterFromTo[Qs,Qs] {
  type exps[FE <: EList] = Full[EList, FE]
}

class QuantSearch[Qs <: QList, Q <: BaseQuantity, I <: Integer, R <: Box[Integer]] {
  type res = R
}

trait CachedConverter

@implicitNotFound(msg = "Cannot convert. Any BaseQuantity which is in the convertee's Quantities but not the converted's must have an exponent of zero.")
class ConvertResult[B <: Box[EList],T <: EList]