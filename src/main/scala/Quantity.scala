package scunits
import shapeless._

trait Quantity
abstract class BaseQuantity(val name: String, val symbol: String) extends Quantity {
  type Basis = this.type :: HNil
}

object Length extends BaseQuantity("length", "L")
object Time extends BaseQuantity("time", "T")
object Mass extends BaseQuantity("mass", "M")
object Temperature extends BaseQuantity("temperature", "Θ")
object AmountOfSubstance extends BaseQuantity("mole", "N")
object ElectricCurrent extends BaseQuantity("electric current", "I")
object LuminousIntensity extends BaseQuantity("luminous intensity", "J")
object Angle extends BaseQuantity("angle", "")
object SolidAngle extends BaseQuantity("solid angle", "")

// abstract class DerivedQuantity[B <: HList](implicit ) extends Quantity

/*abstract class DerivedQuantity[L <: Quantity, R <: Quantity](val left: L, val right: R, op: String)
  extends Quantity("(" + left.name + op + right.name + ")", "(" + left.symbol + op + right.symbol + ")")

class Mul[L <: Quantity, R <: Quantity](l: L, r: R)
  extends DerivedQuantity(l,r,"*")
class Div[L <: Quantity, R <: Quantity](l: L, r: R)
  extends DerivedQuantity(l,r,"/")

object Speed extends Div(Length,Time)
object Acceleration extends Div(Speed,Time)*/
trait Multiplier[L <: Quantity, R <: Quantity, E <: Quantity] {
  def apply(l: Measurement[L], r: Measurement[R]) = Measurement[E](l.v * r.v)
}

object QuantityOps {

}

protected case class Measurement[Q <: Quantity](v: Double) extends AnyVal {
  def +(m: Measurement[Q]) = Measurement(v + m.v)
  def -(m: Measurement[Q]) = Measurement(v - m.v)

  def *[R <: Quantity, E <: Quantity](r: Measurement[R])(implicit m: Multiplier[Q,R,E]): Measurement[E] = m(this, r)
}

object Test {
  import si._
  import shapeless._
  import syntax.singleton._
  import record._

  // type Test = (Length.type ->> Int) :: (Time.type ->> Int) :: HNil
  // def exponent(e: Int) = if(e < 0) (Nat(-e), true) else (Nat(e), false)
  val test = (Length ->> 1) :: (Time ->> -1) :: HNil
}