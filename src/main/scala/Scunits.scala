package scunits

import scunits.quantity._
import scunits.integer._
import scunits.integer.Ops._

case class UnitM[D <: Dimensions](name: String, symbol: String, mult: Double = 1.0, offset: Double = 0.0) {
  protected[scunits] def construct(v: Double) = Measure[D](mult * v + offset)
  def apply(v: Double) = construct(v)
}

class Prefix(val name: String, val symbol: String, val mult: Double) {
  def apply[D <: Dimensions](u: UnitM[D]) = UnitM[D](name + u.name, symbol + u.symbol, mult * u.mult)
  def apply[D <: Dimensions](u: UnitM[D], v: Double) = u.construct(mult * v)
}