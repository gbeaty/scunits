package scunits

import scunits.integer._
import scunits.quantity._

case class Measure[D <: Dims](v: Double) extends AnyVal {
  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[R <: Dims](r: Measure[R]) = Measure[D#Mult[R]](v * r.v)
  def /[R <: Dims](r: Measure[R]) = Measure[D#Div[R]](v / r.v)

  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  def >[R <: Dims](r: Measure[R]) = v > r.v
  def >=[R <: Dims](r: Measure[R]) = v >= r.v
  def <[R <: Dims](r: Measure[R]) = v < r.v
  def <=[R <: Dims](r: Measure[R]) = v <= r.v

  // def format(implicit formatter: UnitM[D]) = unit.format(this)
}