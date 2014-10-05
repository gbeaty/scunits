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

class Converter2 {
  type from <: QList
  type to <: QList  

  type exps[FE <: EList] <: EBox.box
  protected type toDims[es <: EList] = to ^ es
  type apply[D <: Dims] = exps[D#exps]#mapTo[Dims, toDims]
}
class Converter2From[F <: QList] extends Converter2 {
  type from = F
}
@annotation.implicitNotFound(msg = "Cannot find a Converter2 of type ${F} -> ${T}.")
class Converter2FromTo[F <: QList, T <: QList] extends Converter2From[F] {
  type to = T
}
class IndicesConverter2[F <: QList, T <: QList, I <: IList] extends Converter2FromTo[F,T] {
  type indices = I
  type exps[FE <: EList] = indices#convertDims2[FE]
}

class QuantSearch[Qs <: QList, Q <: BaseQuantity, I <: Integer, R <: Box[Integer]] {
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

AB => BC = -0
BC => AB = 1-

AB00 => BC00
AB01 => BC10
AB10 => -
AB11 => -

BC[1,0] = AB[0,1]

AB[0,1] * BC[1,0]
*/