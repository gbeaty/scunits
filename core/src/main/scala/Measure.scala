package scunits

import Scunits._

import scunits.integer._
import scunits.quantity._

protected case class Measure[D <: Dims](v: Double) extends AnyVal {

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  // def *[R <: Dims, A <: Dims](r: Measure[R])(implicit mult: (Measure[D],Measure[R],Mult.type) => Measure[A]) = mult(this, r, Mult)
  // def /[R <: Dims, A <: Dims](r: Measure[R])(implicit div: (Measure[D],Measure[R],Div.type) => Measure[A]) = div(this, r, Div)

  def *[R <: Dims, E <: Dims](r: Measure[R])(implicit mult: Mult[D,R,E]) = Measure[E](v * r.v)
  def /[R <: Dims, E <: Dims](r: Measure[R])(implicit div: Div[D,R,E]) = Measure[E](v / r.v)

  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def ×(r: Double) = Measure[D](v * r)
  def ÷(r: Double) = Measure[D](v / r)

  def >(r: Measure[D]) = v > r.v
  def >=(r: Measure[D]) = v >= r.v
  def <(r: Measure[D]) = v < r.v
  def <=(r: Measure[D]) = v <= r.v

  def ===(r: Measure[D]) = v == r.v
}