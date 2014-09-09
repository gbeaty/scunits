package scunits

import Scunits._

import scunits.integer._
import scunits.quantity._

protected case class Measure[D <: Dims](v: Double) extends AnyVal {

  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: Dims, E <: Dims](r: Measure[R])(implicit mult: Mult[D,R,E]) = Measure[E](v * r.v)
  def /[R <: Dims, E <: Dims](r: Measure[R])(implicit div: Div[D,R,E]) = Measure[E](v / r.v)

  def *(r: Double) = Measure[D](v * r)
  def /(r: Double) = Measure[D](v / r)

  def >(r: Measure[D]) = v > r.v
  def >=(r: Measure[D]) = v >= r.v
  def <(r: Measure[D]) = v < r.v
  def <=(r: Measure[D]) = v <= r.v

  def ===(r: Measure[D]) = v == r.v
}