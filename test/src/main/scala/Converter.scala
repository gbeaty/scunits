package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  import Converter._
  def has[Qs <: Quantities, Q <: BaseQuantity, I <: Integer](in: Qs, find: Q)(implicit i: QuantFound[Qs#quants,Q,I]) = i

  val a = has(ABCDq,A)
  val iA: a.At = new i0

  val b = has(ABCDq,B)
  val iB: b.At = new i1

  val c = has(ABCDq,C)
  val iC: c.At = new i2

  val d = has(ABCDq,D)
  val iD: d.At = new i3
}

object ConverterTests {
  object TestConverters {
    import Converter._
    implicit val aToAb = converter(Aq,ABq)//(indexConverterBuild[An,ABn])
    implicit val bToAb = converter(Bq,ABq)//(indexConverterBuild[Bn,ABn,i1])
    implicit val bToAbc = converter(Bq,ABCq)//(indexConverterBuild[Bn,ABCn,i1])
    implicit val cToAbc = converter(Cq,ABCq)//(indexConverterBuild[Cn,ABCn,i2])
  }
  import TestConverters._

  implicitly[aToAb.Is =:= (i0 -: INil)]
  implicitly[aToAb.Apply[Aq.Dimless] =:= ABq.Dimless]

  implicitly[bToAb.Is =:= (i1 -: INil)]
  implicitly[bToAb.Apply[Bq.Dimless] =:= ABq.Dimless]
  implicitly[bToAb.Apply[Bq.B] =:= ABq.B]

  val aOne = Measure[Aq.A](1.0)
  val abOne = Measure[ABq.A](1.0)

  Measure[aToAb.Apply[Aq.A]](1.0) === abOne
  aToAb(aOne) === abOne
}