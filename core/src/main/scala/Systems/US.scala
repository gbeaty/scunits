package scunits.unit

import scala.math.BigDecimal

import scunits._
import scunits.quantity._
import scunits.unit.si._
import scunits.unit.metric._
import scunits.unit.si.Prefix._
import scunits.unit.time._

class FootBased(metresPerFoot: BigDecimal) {
  val foot = (metre * metresPerFoot).label("foot","ft")

  val link = (foot * 33/50).label("link","li")
  val rod = (foot * 16.5).label("rod","rd")
  val chain = (rod * 4).label("chain","ch")
  val furlong = (chain * 10).label("furlong","fur")
  val inch = (foot / 12.0).label("foot","ft")
  val yard = (foot * 3).label("yard","yd")
  val mile = (foot * 5280).label("mile","mi")
  val league = (mile * 3).label("league","lea")

  val squareInch = inch.sq
  val squareFoot = foot.sq
  val squareChain = chain.sq
  val acre = (squareFoot * 43560).label("acre","acre")
  val squareMile = mile.sq
  val section = squareMile.label("section","section")
  val township = (section * 36).label("township","twp")

  val cubicInch = inch.cu
  val cubicFoot = foot.cu
  val cubicYard = yard.cu
  val acreFoot = (acre * foot).label("acre-foot","acre-ft")

  // val pound = UnitM[Mass]("pound", "lb", 0.45359237)
  val poundForce = (newton * 4.4482216152605).label("pound force", "lbf")

  val psi = (poundForce / squareInch).label("pounds per square inch","psi")

  val mph = (mile / hour).label("miles per hour","mph")
  val fps = (foot / second).label("feet per second","fps")
}

object us extends FootBased(0.3048) {
  val fahrenheit = UnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)

  object survey extends FootBased(0.30480061)

  // Nautical:
  val fathom = (yard * 2).label("fathom","ftm")
  val cable = (fathom * 120).label("cable","cb")
  val nauticalMile = (us.mile * 1.151).label("nautical mile","nmi")

  // Volume:
  object fluid {
    val minim = (micro(litre) * 61.611519921875).label("minim","min")
    val dram = (minim * 60).label("fluid dram","fl dr")
    val teaspoon = (minim * 80).label("teaspoon","tsp")
    val tablespoon = (teaspoon * 3).label("tablespoon","Tbsp")
    val ounce = (tablespoon * 2).label("fluid ounce","fl oz")
    val shot = (tablespoon * 3).label("shot","jig")
    val gill = (ounce * 4).label("gill","gi")
    val cup = (gill * 2).label("cup","cp")
    val pint = (cup * 2).label("pint","pt")
    val quart = (pint * 2).label("quart","qt")
    val gallon = (quart * 4).label("gallon","gal")
    val barrel = (gallon * 31.5).label("barrel","bbl")
    val oilBarrel = (gallon * 42).label("oil barrel","bbl")
    val hogshead = (gallon * 63).label("hogshead","hogshead")
  }

  object dry {
    val pint = (litre * 0.5506104713575).label("pint","pt")
    val quart = (pint * 2).label("quart","qt")
    val gallon = (quart * 4).label("gallon","gal")
    val peck = (gallon * 2).label("peck","pk")
    val bushel = (peck * 4).label("bushel","bu")
    val barrel = (cubicInch * 7056).label("barrel","bbl")
  }

  val grain = (gram * 0.06479891).label("grain","gr")
  val dram = (grain * (27 + 11/32)).label("dram","dr")
  val ounce = (dram * 16).label("ounce","oz")
  val pound = (ounce * 16).label("pound","lb")
  val hundredweight = (pound * 100).label("hundredweight","cwt")
  val longHundredweight = (pound * 112).label("long hundredweight","long cwt")
  val ton = (hundredweight * 20).label("ton","ton")
  val longTon = (longHundredweight * 20).label("long ton","long ton")

  object troy {
    val grain = us.grain
    val pennyweight = (grain * 24).label("pennyweight","dwt")
    val ounce = (pennyweight * 20).label("troy ounce","oz t")
    val pound = (ounce * 12).label("troy pound","lb t")
  }

  val mpg = (mile / fluid.gallon).label("miles per gallon","mpg")
}