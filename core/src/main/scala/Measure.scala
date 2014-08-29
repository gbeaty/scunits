package scunits

import scunits.integer._
import scunits.quantity._

protected case class Measure[D <: Dimensions](v: Double) extends AnyVal {
  def +(m: Measure[D]) = Measure[D](v + m.v)
  def -(m: Measure[D]) = Measure[D](v - m.v)

  def *[RD <: Dimensions](r: Measure[RD]) = Measure[D#Mult[RD]](v * r.v)
  def /[RD <: Dimensions](r: Measure[RD]) = Measure[D#Div[RD]](v * r.v)

  // def format(implicit formatter: UnitM[D]) = unit.format(this)
}