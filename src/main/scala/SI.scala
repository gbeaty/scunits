package scunits

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

  /*val meter = UnitM[Length]("meter", "m")
  val gram = UnitM[Mass]("gram", "g")  
  val kelvin = UnitM[Temperature]("kelvin", "K")
  val celcius = UnitM[Temperature]("celsius", "", offset = 273.15)
  val ampere = UnitM[Electric.Current]("ampere", "A")
  val mole = UnitM[AmountOfSubstance]("mole", "mol")
  val candela = UnitM[Luminous.Intensity]("candela", "cd")
  val hertz = UnitM[Frequency]("hertz","Hz")
  val newton = UnitM[Force]("newton","N")
  val pascal = UnitM[Pressure]("pascal","Pa")
  val joule = UnitM[Energy]("joule","J")
  val watt = UnitM[Power]("watt","W")
  val coulomb = UnitM[Electric.Charge]("coulomb","C")
  val volt = UnitM[Electric.Potential]("volt","V")
  val farad = UnitM[Electric.Capacitance]("farad","F")
  val ohm = UnitM[Electric.Resistance]("ohm","Ω")
  val siemens = UnitM[Electric.Conductance]("siemens","S")
  val weber = UnitM[Magnetic.Flux]("weber","Wb")
  val tesla = UnitM[Magnetic.FieldStrength]("tesla","T")
  val henry = UnitM[Electric.Inductance]("henry","H")
  val lumen = UnitM[Luminous.Intensity]("lumen","lm")
  val lux = UnitM[Illuminance]("lux","lx")
  val becquerel = UnitM[Radioactive.Decay]("becquerel","Bq")
  val gray = UnitM[Radioactive.Dose]("gray","Gy")
  val sievert = UnitM[Radioactive.Dose]("sievert","Sv")
  val katal = UnitM[CatalyticActivity]("katal","kat")*/
}