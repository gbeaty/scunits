package scunits

import scunits.quantity._
import scunits.integer._
import scunits.integer.Ops._

trait UnitM[D <: Dims] {
  type Dimensions = D
  val name: String
  val symbol: String

  def apply(in: Double): Measure[D]
  def unapply(out: Measure[D]): Double
}

case class BaseUnitM[D <: Dims](name: String, symbol: String) extends UnitM[D] with LinearUnitM[D] {
  def apply(in: Double) = Measure[D](in)
  def unapply(out: Measure[D]) = out.v
  val toSIMult = 1.0
}
sealed trait NonSIUnitM[D <: Dims] extends UnitM[D] {
  def toSI(in: Double): Double
  def fromSI(out: Double): Double
  def apply(in: Double) = Measure[D](toSI(in))
  def unapply(out: Measure[D]) = fromSI(out.v)
}
sealed trait LinearUnitM[D <: Dims] extends UnitM[D] {
  val toSIMult: Double
  def mult[R <: Dims](n: String, s: String, r: LinearUnitM[R]) = MultUnitM[D#Mult[R]](n, s, toSIMult * r.toSIMult)
  def div[R <: Dims](n: String, s: String, r: LinearUnitM[R]) = MultUnitM[D#Div[R]](n, s, toSIMult / r.toSIMult)
}
case class MultUnitM[D <: Dims](name: String, symbol: String, toSIMult: Double) extends LinearUnitM[D] with NonSIUnitM[D] {
  def toSI(in: Double) = in * toSIMult
  def fromSI(out: Double) = out / toSIMult
}
case class OffsetUnitM[D <: Dims](name: String, symbol: String, toSIOffset: Double) extends LinearUnitM[D] with NonSIUnitM[D] {
  final val toSIMult = 1.0
  def toSI(in: Double) = in + toSIOffset
  def fromSI(out: Double) = out - toSIOffset
}
case class MultOffsetUnitM[D <: Dims](name: String, symbol: String, toSIMult: Double, toSIOffset: Double) extends LinearUnitM[D] with NonSIUnitM[D] {
  def toSI(in: Double) = (in + toSIOffset) * toSIMult
  def fromSI(out: Double) = (out / toSIMult) - toSIOffset
}

class Prefix(val name: String, val symbol: String, val mult: Double) {
  def apply[D <: Dims](u: LinearUnitM[D]) = MultUnitM[D](name + u.name, symbol + u.symbol, mult * u.toSIMult)
  def apply[D <: Dims](u: LinearUnitM[D], v: Double) = u(mult * v)
}

trait Formatter extends Function2[Double,UnitM[_],String] {
  def apply(v: Double, u: UnitM[_]): String
}