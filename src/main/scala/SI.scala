package scunits

package object si {

  object Prefixes {
    val yocto = new Prefix("yocto", "y", 1e-24)
    val zepto = new Prefix("zepto", "z", 1e-21)
    val atto = new Prefix("atto", "a", 1e-18)
    val femto = new Prefix("femto", "f", 1e-15)
    val pico = new Prefix("pico", "p", 1e-12)
    val nano = new Prefix("nano", "n", 1e-9)
    val micro = new Prefix("micro", "µ", 1e-6)
    val milli = new Prefix("milli", "m", 1e-3)
    val centi = new Prefix("centi", "c", 1e-2)
    val deci = new Prefix("deci", "d", 1e-1)
    val deka = new Prefix("deka", "da", 1e1)
    val hecto = new Prefix("hecto", "h", 1e2)
    val kilo = new Prefix("kilo", "k", 1e3)
    val mega = new Prefix("mega", "M", 1e6)
    val giga = new Prefix("giga", "G", 1e9)
    val tera = new Prefix("tera", "T", 1e12)
    val peta = new Prefix("peta", "P", 1e15)
    val exa = new Prefix("exa", "E", 1e18)
    val zetta = new Prefix("zetta", "Z", 1e21)
    val yotta = new Prefix("yotta", "Y", 1e24)
  }

  val meter = UnitM[Length.type]("meter", "m")
  val gram = UnitM[Mass.type]("gram", "g")  
  val kelvin = UnitM[Temperature.type]("kelvin", "K")
  val celcius = UnitM[Temperature.type]("celsius", "", offset = 273.15)
  val ampere = UnitM[ElectricCurrent.type]("ampere", "A")
  val mole = UnitM[AmountOfSubstance.type]("mole", "mol")
  val candela = UnitM[LuminousIntensity.type]("candela", "cd")

  import Prefixes._
  val test = micro(meter, 1000)
}