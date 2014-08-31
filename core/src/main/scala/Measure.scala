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

  def ===(r: Measure[D]) = v == r.v

  def multDenom[R <: Dims](r: Measure[R#Div[D]]) = Measure[R](v * r.v)
  def divNom[R <: Dims](r: Measure[D#Div[R]]) = Measure[R](v / r.v)

  // def format(implicit formatter: UnitM[D]) = unit.format(this)
}