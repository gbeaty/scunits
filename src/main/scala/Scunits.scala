import scunits.tlist._
import scunits.integer._

package object scunits {
  type BaseQuantities = TEl[BaseQuantity]
  type Magnitudes = TEl[Integer]
  type DimensionsOf[Q <: BaseQuantities] = Dimensions[Q, _ <: TEl[Integer]]

  class BaseQuantity(val name: String, val symbol: String)

  trait Dimensions[Q <: BaseQuantities, M <: Magnitudes] {
    type Quantities = Q
    type Mags = M
    type Mult[R <: DimensionsOf[Q]] = Dimensions[Q,M#Zip[R#Mags,Integer]#Map[Op[+]]]
    type Div[R <: DimensionsOf[Q]] = Dimensions[Q,M#Zip[R#Mags,Integer]#Map[Op[-]]]
  }

  case class UnitM[Q](name: String, symbol: String, mult: Double = 1.0, offset: Double = 0.0) {

    def construct(v: Double) = Measure(v)

    def apply(v: Double) = construct(mult * v + offset)  
  }

  class Prefix(val name: String, val symbol: String, val mult: Double) {
    def apply[Q](u: UnitM[Q]) = UnitM[Q](name + u.name, symbol + u.symbol, mult * u.mult)
    def apply[Q](u: UnitM[Q], v: Double) = u.construct(mult * u.mult * v)
  }
}