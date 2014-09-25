package scunits.types

import scunits._
import TestTypes._

object SearchTests {
  import Converter._
  def index[A <: QList, Q <: Quantity, I <: Integer](implicit i: QuantFoundAt[A,Q,I]) = i

  val qf: QuantFoundAt[ABCDn,B,i1] = quantSearch[ABCDn,B,i1]
  val inOfA = index[ABCDn,A,i0]
  val inOfB = index[ABCDn,B,i1]
  val inOfC = index[ABCDn,C,i2]
  val inOfD = index[ABCDn,D,i3]

  val a: inOfA.At = new i0
  val b: inOfB.At = new i1
}

object ConverterTests {
  object TestConverters {
    import Converter._
    implicit val aToAb = converter(Aq,ABq)//(indexConverterBuild[An,ABn])
    implicit val bToAb = converter(Bq,ABq)(
      indexConverterBuild[Bn,ABn](
        quantSearch[ABn,B,i1], indexConverterBuilt[ABn]))
  }
  import TestConverters._

  implicitly[aToAb.Is =:= (i0 -: INil)]
  implicitly[aToAb.Apply[Aq.Dimless] =:= ABq.Dimless]

  implicitly[bToAb.Is =:= (i1 -: INil)]
  implicitly[bToAb.Apply[Bq.Dimless] =:= ABq.Dimless]
  implicitly[bToAb.Apply[Bq.B] =:= ABq.B]
  
  // val b1: bToAb.Apply[Bq.B] = 1
  // val b2: ABq.B = 1
  // ^[::[TestTypes.A.type,::[TestTypes.B.type,QNil]],*:[i1,*:[i1,DNil]]]
  // ^[::[TestTypes.A.type,::[TestTypes.B.type,QNil]],*:[i0,*:[i1,DNil]]]

  val aOne = Measure[Aq.A](1.0)
  val abOne = Measure[ABq.A](1.0)
  // Measure[aToAb.Apply[Aq.A]](1.0) === abOne
  // aOne === abOne
  // val a: aToAb.Exps[i1 *: DNil] = 1
}