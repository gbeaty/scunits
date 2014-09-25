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
    implicit val aToAb = converter(Aq,ABq)(indexConverterBuild[An,ABn,i0])
    // implicit val abdToAbcd = converter(ABDq,ABCDq)(indexConverterBuild[ABDn,ABCDn,i0])
  }
  import TestConverters._

  implicitly[aToAb.Apply[Aq.Dimless] =:= ABq.Dimless]
  // val a: aToAb.Is = 1

  val aOne = Measure[Aq.A](1.0)
  val abOne = Measure[ABq.A](1.0)
  // Measure[aToAb.Apply[Aq.A]](1.0) === abOne
  // aOne === abOne
  // val a: aToAb.Exps[i1 *: DNil] = 1
}