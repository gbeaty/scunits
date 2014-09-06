package scunits.unit.us

import scala.math.BigDecimal

import scunits._
import scunits.quantity._
import scunits.unit.si.base._

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

  val psi = (pascal * 6894.757293168).label("pounds per square inch","psi")

  val mph = (mile / hour).label("miles per hour","mph")
  val fps = (foot / second).label("feet per second","fps")
}
class InternationalFoot extends FootBased(0.3048)
class SurveyFoot extends FootBased(0.30480061)

package object volume {
  trait Fluid {
    val minim = (cubicMetre * 6.1611519921875e-8).label("minim","min")
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
  trait Dry {
    val cubicInch: UnitM[Length]
    val pint = (cubicMetre * 0.0005506104713575).label("pint","pt")
    val quart = (pint * 2).label("quart","qt")
    val gallon = (quart * 4).label("gallon","gal")
    val peck = (gallon * 2).label("peck","pk")
    val bushel = (peck * 4).label("bushel","bu")
    val barrel = (cubicInch * 7056).label("barrel","bbl")
  }
}

trait Nautical {
  val fathom = (metre * 1.8288).label("fathom","ftm")
  val cable = (fathom * 120).label("cable","cb")
  val nauticalMile = (metre * 1852).label("nautical mile","nmi")
  val knot = (nauticalMile / hour).label("knot","kt")
}

package object mass {
  trait Avoirdupois {
    val grain = (gram * 0.06479891).label("grain","gr")
    val dram = (grain * (27 + 11/32)).label("dram","dr")
    val ounce = (dram * 16).label("ounce","oz")
    val pound = (ounce * 16).label("pound","lb")
    val hundredweight = (pound * 100).label("hundredweight","cwt")
    val longHundredweight = (pound * 112).label("long hundredweight","long cwt")
    val ton = (hundredweight * 20).label("ton","ton")
    val longTon = (longHundredweight * 20).label("long ton","long ton")
  }
  trait Troy {
    val grain = (gram * 0.06479891).label("troy grain","gr")
    val pennyweight = (grain * 24).label("pennyweight","dwt")
    val ounce = (pennyweight * 20).label("troy ounce","oz t")
    val pound = (ounce * 12).label("troy pound","lb t")
  }
}

trait Base {
  val fahrenheit = UnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)
  val poundForce = (newton * 4.4482216152605).label("pound force", "lbf")
  val BTU = (joule * 1055.056).label("British thermal unit","BTU")
  val calorie = (joule * 4.184).label("calorie","cal")
  val hand = (metre * 0.1016).label("hand","h")
  val lbft = (newtonMetre * 1.3558179483314004).label("pound-foot","lb-ft")
  val bhp = Horsepower.mechanical
  object Horsepower {
    val mechanical = ((lbft * 33000) / minute).label("mechnical horsepower","hp(I)")
    val electric = (watt * 746).label("electric horsepower","hp(E)")
    val boiler = (watt * 9812.5).label("boiler horsepower","hp(S)")
  }
  val mpg = ((metre / squareMetre) * 425143.707).label("miles per gallon","mpg")
}

trait All extends InternationalFoot with Nautical with mass.Avoirdupois with Base {
  object Survey extends SurveyFoot
  object Fluid extends volume.Fluid
  object Dry extends volume.Fluid
  object Troy extends mass.Troy
}
package object all extends All