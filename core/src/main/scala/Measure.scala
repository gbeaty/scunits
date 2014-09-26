package scunits

import scunits.types._

case class Measure[D <: Dims](v: Double) extends AnyVal with Ordered[Measure[D]] {
  type dims = D

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#mult[R]](r.v * v)
  def /[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#div[R]](r.v / v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)
  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def compare(that: Measure[D]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#neg](1.0 / v)
}

trait MeasLike extends Any {
  val v: Double
  type quants <: QList
  type exps <: DList

  type mult[R <: MeasOf[quants]] = Meas[quants, exps#mult[R#exps]]
  type div[R <: MeasOf[quants]] = Meas[quants, exps#mult[R#exps#neg]]
  type neg = Meas[quants,exps#neg]
}
trait MeasOf[Q <: QList] extends Any with MeasLike {
  type quants = Q
}
case class Meas[Q <: QList, E <: DList](v: Double) extends AnyVal with MeasOf[Q] {
  type exps = E

  def +(m: Meas[Q,E]) = Meas[Q,E](v + m.v)
  def -(m: Meas[Q,E]) = Meas[Q,E](v - m.v)

  def *[R <: MeasOf[Q]](r: R) = new mult[R](r.v * v)
  def /[R <: MeasOf[Q]](r: R) = new div[R](r.v / v)

  def ×(r: Double) = Meas[Q,E](v * r)
  def ÷(r: Double) = Meas[Q,E](v / r)
  def mult(r: Double) = Meas[Q,E](v * r)
  def div(r: Double) = Meas[Q,E](v / r)

  def compare(that: Meas[Q,E]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Meas[Q,E]) = v == r.v

  def inv = new neg(1.0 / v)
}