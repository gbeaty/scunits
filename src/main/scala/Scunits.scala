package scunits

case class UnitM[Q <: Quantity](name: String, symbol: String, mult: Double = 1.0, offset: Double = 0.0) {

  def construct(v: Double) = Measurement[Q](v)

  def apply(v: Double) = construct(mult * v + offset)  
}

class Prefix(val name: String, val symbol: String, val mult: Double) {
  def apply[Q <: Quantity](u: UnitM[Q]) = UnitM[Q](name + u.name, symbol + u.symbol, mult * u.mult)
  def apply[Q <: Quantity](u: UnitM[Q], v: Double) = u.construct(mult * u.mult * v)
}