package scunits

import scunits.types._

case class Measure[D <: Dims](v: Double) extends AnyVal with Ordered[Measure[D]] {
  type Dims = D

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: DimsOf[D#Quants]](r: Measure[R]) = Measure[D#Mult[R]](r.v * v)
  def /[R <: DimsOf[D#Quants]](r: Measure[R]) = Measure[D#Div[R]](r.v / v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)
  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def compare(that: Measure[D]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#Neg](1.0 / v)
}