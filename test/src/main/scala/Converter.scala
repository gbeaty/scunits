package scunits.types

import scunits._
import TestTypes._

object ConverterTests {
  import Converter._
  def index[A <: QList, Q <: Quantity, I <: NonNegInt](implicit i: QuantIndex[A,Q] { type Index = I }) = i

  index[ABCDn,A,i0]
  index[ABCDn,B,i1]
  index[ABCDn,C,i2]
  index[ABCDn,D,i3]
}

object MeasureTests {
  implicit val nilToAb = converter[Nilq,ABq]

  val nilOne = Measure[Nilq.Dimless](1.0)
  val abOne = Measure[ABq.Dimless](1.0)
  nilOne * nilOne
  abOne * abOne
  abOne === (nilOne * abOne)

  /*Measure[TestNilq.Dimless#Mult[MeasureTests.nilToAb.Apply[TestNilq.Dimless]]]  
  Measure[TestABq.Dimless]

  Measure[^[QNil,MeasureTests.nilToAb.Exps[DNil]#OpNil[[L <: Integer, R <: Integer]L#Add[R]]]]
  Measure[^[::[TestA.type,::[TestB.type,QNil]],DNil]]*/
}