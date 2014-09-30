package scunits

import scunits.types._

case class Measure[D <: Dims](v: Double) extends AnyVal with Ordered[Measure[D]] {
  type dims = D
  type Conv[R <: Dims] = ConverterFrom[R#quants] with CachedConverter

  // def +[R <: Dims,C <: Conv[R]](m: Measure[R])(implicit c: C) = Measure[c.apply[D]](v + m.v)
  // def -[R <: Dims,C <: Conv](m: Measure[R])(implicit c: C) = Measure[c.apply[D]](v - m.v)
  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#mult[R]](v * r.v)
  def /[R <: DimsOf[D#quants]](r: Measure[R]) = Measure[D#div[R]](v / r.v)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)
  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def compare(that: Measure[D]) = if(v < that.v) -1 else if(v > that.v) 1 else 0

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#neg](1.0 / v)
}