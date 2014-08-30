package scunits.unit

import scunits._
import scunits.quantity._

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

  val metre = BaseUnitM[Length]("meter", "m")
  val gram = BaseUnitM[Mass]("gram", "g")  
  val kelvin = BaseUnitM[Temperature]("kelvin", "K")
  val celcius = OffsetUnitM[Temperature]("celsius", "°C", 273.15)
  val ampere = BaseUnitM[Electric.Current]("ampere", "A")
  val mole = BaseUnitM[AmountOfSubstance]("mole", "mol")
  val candela = BaseUnitM[Luminous.Intensity]("candela", "cd")
  val hertz = BaseUnitM[Frequency]("hertz","Hz")
  val newton = BaseUnitM[Force]("newton","N")
  val pascal = BaseUnitM[Pressure]("pascal","Pa")
  val joule = BaseUnitM[Energy]("joule","J")
  val watt = BaseUnitM[Power]("watt","W")
  val coulomb = BaseUnitM[Electric.Charge]("coulomb","C")
  val volt = BaseUnitM[Electric.Potential]("volt","V")
  val farad = BaseUnitM[Electric.Capacitance]("farad","F")
  val ohm = BaseUnitM[Electric.Resistance]("ohm","Ω")
  val siemens = BaseUnitM[Electric.Conductance]("siemens","S")
  val weber = BaseUnitM[Magnetic.Flux]("weber","Wb")
  val tesla = BaseUnitM[Magnetic.FieldStrength]("tesla","T")
  val henry = BaseUnitM[Electric.Inductance]("henry","H")
  val lumen = BaseUnitM[Luminous.Intensity]("lumen","lm")
  val lux = BaseUnitM[Illuminance]("lux","lx")
  val becquerel = BaseUnitM[Radioactive.Decay]("becquerel","Bq")
  val gray = BaseUnitM[Radioactive.Dose]("gray","Gy")
  val sievert = BaseUnitM[Radioactive.Dose]("sievert","Sv")
  val katal = BaseUnitM[CatalyticActivity]("katal","kat")

  val squareMetre = metre.mult("square metre", "m2", metre)
  val cubicMetre = squareMetre.mult("cubic metre", "m3", metre)
}