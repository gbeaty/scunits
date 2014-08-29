package scunits

import scunits.quantity._
import scunits.integer._
import scunits.integer.Ops._

case class UnitM[D <: Dimensions](name: String, symbol: String, toSI: Double => Double) {
  protected[scunits] def construct(v: Double) = Measure[D](toSI(v))
  def apply(v: Double) = construct(v)
}
object UnitM {
  def apply[D <: Dimensions](name: String, symbol: String, mult: Double = 1.0, offset: Double = 0.0): UnitM[D] =
    UnitM[D](name, symbol, (i: Double) => i * mult + offset)
}

class Prefix(val name: String, val symbol: String, val mult: Double) {
  def apply[D <: Dimensions](u: UnitM[D]) = UnitM[D](name + u.name, symbol + u.symbol, (i: Double) => u.toSI(mult * i))
  def apply[D <: Dimensions](u: UnitM[D], v: Double) = u.construct(mult * v)
}

trait Formatter extends Function2[Double,UnitM[_],String] {
  def apply(v: Double, u: UnitM[_]): String
}

/*case class FixedNumberFormatter(maxDigits: Int, maxDecimals: Int, minDecimals: Int) extends Formatter {
  def apply(v: Double, u: UnitM[_]) = 
}*/