package scunits

import scunits.integer._

case class Measure[D <: Dims](v: Double) extends AnyVal {

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: Dims, A <: Dims](r: Measure[R])(implicit mult: (Measure[D],Measure[R],Mult.type) => Measure[A]) = mult(this, r, Mult)
  def /[R <: Dims, A <: Dims](r: Measure[R])(implicit div: (Measure[D],Measure[R],Div.type) => Measure[A]) = div(this, r, Div)

  def *(r: Double) = Measure[D](v * r)
  def /(r: Double) = Measure[D](v / r)

  def >(r: Measure[D]) = v > r.v
  def >=(r: Measure[D]) = v >= r.v
  def <(r: Measure[D]) = v < r.v
  def <=(r: Measure[D]) = v <= r.v

  def ===(r: Measure[D]) = v == r.v

  def inv = Measure[D#Neg](1.0 / v)
}