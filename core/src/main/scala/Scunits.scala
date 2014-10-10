import scunits._
import scunits.types._

trait LowPriorityImplicits {  
}

package object scunits extends LowPriorityImplicits {
  implicit def invertMeasure[D <: Dims](m: Measure[D]) = m.inv

  // Converting:
  implicit def convert[Conv <: ConverterFrom[D#quants] with CachedConverter, D <: Dims, R <: EList](m: Measure[D])
    (implicit c: Conv, ic: ConvertResult[Conv#exps[D#exps],R]) = Measure[Conv#to ^ R](m.v)

  implicit def convertResult[C <: EList]: ConvertResult[Full[EList,C],C] = null

  def converter[F <: Quantities, T <: Quantities, Is <: IList](f: F, t: T)
    (implicit c: IndicesConverter[F#quants,T#quants,Is]) =
      new IndicesConverter[F#quants,T#quants,Is] with CachedConverter
  
  // Quantity searching:
  implicit def quantSearch[Qs <: QNel, Q <: BaseQuantity, R <: Box[Integer], I <: Integer]
    (implicit ts: QuantSearch[Qs#tail,Q,I#succ,R]): QuantSearch[Qs,Q,I,R] = null

  implicit def quantFound[Qs <: QNelOf[Q], Q <: BaseQuantity, I <: Integer]:
    QuantSearch[Qs,Q,I,Full[Integer,I]] = null

  implicit def quantNotfound[Q <: BaseQuantity, I <: Integer]:
    QuantSearch[QNil,Q,I,Empty[Integer]] = null

  // Converter construction:
  implicit def indexConverterBuild[F <: QNel, T <: QList, R <: Box[Integer], Is <: IList]
    (implicit i: QuantSearch[T,F#head,_0,R], c: IndicesConverter[F#tail,T,Is]): IndicesConverter[F,T,R =: Is] = null

  implicit def indexConverterBuilt[T <: QList]: IndicesConverter[QNil,T,INil] = null
}