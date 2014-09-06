package scunits.unit.si

import scunits._
import scunits.quantity._

trait Prefix {
  val yocto = new UnitPrefix("yocto", "y", 1e-24)
  val zepto = new UnitPrefix("zepto", "z", 1e-21)
  val atto = new UnitPrefix("atto", "a", 1e-18)
  val femto = new UnitPrefix("femto", "f", 1e-15)
  val pico = new UnitPrefix("pico", "p", 1e-12)
  val nano = new UnitPrefix("nano", "n", 1e-9)
  val micro = new UnitPrefix("micro", "µ", 1e-6)
  val milli = new UnitPrefix("milli", "m", 1e-3)
  val centi = new UnitPrefix("centi", "c", 1e-2)
  val deci = new UnitPrefix("deci", "d", 1e-1)
  val deka = new UnitPrefix("deka", "da", 1e1)
  val hecto = new UnitPrefix("hecto", "h", 1e2)
  val kilo = new UnitPrefix("kilo", "k", 1e3)
  val mega = new UnitPrefix("mega", "M", 1e6)
  val giga = new UnitPrefix("giga", "G", 1e9)
  val tera = new UnitPrefix("tera", "T", 1e12)
  val peta = new UnitPrefix("peta", "P", 1e15)
  val exa = new UnitPrefix("exa", "E", 1e18)
  val zetta = new UnitPrefix("zetta", "Z", 1e21)
  val yotta = new UnitPrefix("yotta", "Y", 1e24)
}

trait Base {
  val metre = UnitM[Length]("meter", "m")
  val second = UnitM[Time]("second","s")  
  val gram = UnitM[Mass]("gram", "g")
  val kelvin = UnitM[Temperature]("kelvin", "K")
  val celsius = UnitM[Temperature]("celsius", "°C", 1.0, 273.15)
  val ampere = UnitM[Electric.Current]("ampere", "A")
  val mole = UnitM[AmountOfSubstance]("mole", "mol")
  val candela = UnitM[Luminous.Intensity]("candela", "cd")
  val hertz = UnitM[Frequency]("hertz","Hz")

  val minute = (second * 60.0).label("second","s")
  val hour = (minute * 60.0).label("hour","hr")

  val metrePerSecond = UnitM[Speed]("metre per second", "m/s")
  val metrePerSecondSquared = UnitM[Acceleration]("metre per second squared", "m/s^2")
  val newton = UnitM[Force]("newton","N")
  val newtonMetre = newton * metre
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
  val katal = UnitM[CatalyticActivity]("katal","kat")

  val jouleSecond = UnitM[AngularMomentum]("Joule-second","J*s")

  val squareMetre = UnitM[Area]("square metre", "m^2")
  val cubicMetre = UnitM[Volume]("cubic metre", "m^3")  
}

package object base extends Base
package object prefix extends Prefix

trait Accepted {
  import base._
  val litre = (cubicMetre / 1000).label("litre","L")
  val tonne = (gram * 1000000).label("tonne","t")
  val electronVolt = (joule * 1.60217656535e-19).label("electron volt","eV")
  val dalton = (gram * 1.66053892173e-24).label("dalton","Da")
  val astroUnit = (metre * 1.495978706916e11).label("astronomical unit","au")
  val speedOfLight = (metrePerSecond * 299792458).label("speed of light","c")
  val reducedPlankConstant = (joule * second * 1.0545716818e34).label("reduced plank constant","ħ")
  val electronMass = (gram * 9.109382616e-28).label("electron mass","me")    
}

package object accepted extends Accepted
package object all extends Base with Prefix with Accepted