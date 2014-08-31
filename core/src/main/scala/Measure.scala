package scunits

import scunits.integer._
import scunits.quantity._

case class Measure[D <: Dims](v: Double) extends AnyVal {
  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[RD <: Dims](r: Measure[RD]) = Measure[D#Mult[RD]](v * r.v)
  def /[RD <: Dims](r: Measure[RD]) = Measure[D#Div[RD]](v / r.v)

  def mult(r: Double) = Measure[D](v * r)
  def div(r: Double) = Measure[D](v / r)

  // def format(implicit formatter: UnitM[D]) = unit.format(this)
}