package scunits

abstract class Quantity(val name: String, val symbol: String)

object Length extends Quantity("length", "L")
object Time extends Quantity("time", "T")
object Mass extends Quantity("mass", "M")
object Temperature extends Quantity("temperature", "Θ")

case class Measurement[V <: AnyVal, Q <: Quantity](v: V) extends AnyVal

class UnitM[Q <: Quantity](val symbol: String, val mult: Double) {

  def construct[V <: AnyVal](v: V) = Measurement[V,Q](v)

  def apply(v: Double) = construct(mult * v)
  def apply(v: Int) = construct((mult * v).toInt)
  def apply(v: Short) = construct((mult * v).toShort)
  def apply(v: Byte) = construct((mult * v).toByte)
}

/*trait Measurement[A <: AnyVal, Q <: Quantity] extends Any {
  def quantity: Quantity
  val v: A

  def format[U <: UnitM[Q]](unit: U) = v + unit.symbol
}

protected class Length[V <: AnyVal](val v: V) extends AnyVal with Measurement[V,Quantity.Length.type] {
  def quantity = Quantity.Length
}*/

/*case class Prefix[Q <: Quantity](owner: UnitM[Q], name: String, prefix: String, mult: Double) extends UnitM[Q] {
  val symbol = prefix + owner.symbol
}*/

class SiUnits(mult: Double) {
  /*case class SIUnit[Q <: Quantity](symbol: String, mult: Double) extends UnitM[Q] {
    val milli = Prefix(this, "milli", "m", 1000.0)
  }*/

  val meter = new UnitM[Length.type]("m", mult)
}

class SiPrefixedUnits extends SiUnits(1.0) {
  val yocto = new SiUnits(1e-24)
  val zepto = new SiUnits(1e-21)
  val atto = new SiUnits(1e-18)
  val femto = new SiUnits(1e-15)
  val pico = new SiUnits(1e-12)
  val nano = new SiUnits(1e-9)
  val micro = new SiUnits(1e-6)
  val milli = new SiUnits(1e-3)
  val centi = new SiUnits(1e-2)
  val deci = new SiUnits(1e-1)
  val deca = new SiUnits(1e1)
  val hecto = new SiUnits(1e2)
  val kilo = new SiUnits(1e3)
  val mega = new SiUnits(1e6)
  val giga = new SiUnits(1e9)
  val tera = new SiUnits(1e12)
  val peta = new SiUnits(1e15)
  val exa = new SiUnits(1e18)
  val zetta = new SiUnits(1e21)
  val yotta = new SiUnits(1e24)
}