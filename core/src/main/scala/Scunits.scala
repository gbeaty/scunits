import scunits._
import scunits.types._

trait LowPriorityImplicits {
  implicit def identityConverter[Qs <: QList] = new IdentityConverter[Qs] with CachedConverter

  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, R <: Box[Integer], I <: Integer]
    (implicit ts: QuantSearch[Qs#tail,Q,I#succ,R]): QuantSearch[Qs,Q,I,R] = null
}

package object scunits extends LowPriorityImplicits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv

  implicit def convert[C <: ConverterFrom[D#quants] with CachedConverter, D <: Dims](m: Measure[D])
    (implicit c: C) = Measure[C#apply[D]](m.v)

  def convert2[Conv <: Converter2From[D#quants] with CachedConverter, D <: Dims](m: Measure[D])
    (implicit c: Conv/*, f: IsFull[Conv#apply[D]]*/) = identity(1)// = Measure[f.contents](m.v)
  
  def converter[F <: Quantities, T <: Quantities, Is <: IList](f: F, t: T)
    (implicit c: IndicesConverter[F#quants,T#quants,Is]) =
      new IndicesConverter[F#quants,T#quants,Is] with CachedConverter

  def converter2[F <: Quantities, T <: Quantities, Is <: IList](f: F, t: T)
    (implicit c: IndicesConverter2[F#quants,T#quants,Is]) =
      new IndicesConverter2[F#quants,T#quants,Is] with CachedConverter
  
  implicit def quantFound[Qs <: QNelOf[Q], Q <: BaseQuantity, I <: Integer]:
    QuantSearch[Qs,Q,I,IntBox.full[I]] = null
  implicit def quantNotfound[Q <: BaseQuantity, I <: Integer]:
    QuantSearch[QNil,Q,I,IntBox.empty] = null

  implicit def indexConverterBuild[F <: QNel, T <: QList, R <: Box[Integer], Is <: IList]
    (implicit i: QuantSearch[T,F#head,_0,R], c: IndicesConverter[F#tail,T,Is]): IndicesConverter[F,T,R =: Is] = null

  implicit def indexConverterBuilt[T <: QList]: IndicesConverter[QNil,T,INil] = null

  implicit def indexConverter2Build[F <: QNel, T <: QList, R <: Box[Integer], Is <: IList]
    (implicit i: QuantSearch[T,F#head,_0,R], c: IndicesConverter2[F#tail,T,Is]): IndicesConverter2[F,T,R =: Is] = null

  implicit def indexConverter2Built[T <: QList]: IndicesConverter2[QNil,T,INil] = null
}