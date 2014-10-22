package scunits.system.us

import scala.math.BigDecimal

import scunits._

class FootBased(metresPerFoot: BigDecimal) {
  // val foot = UnitM[Length]("foot","ft",metresPerFoot)

  // val link = (foot * 33/50).label("link","li")
  // val rod = (foot * 16.5).label("rod","rd")
  // val chain = (rod * 4).label("chain","ch")
  // val furlong = (chain * 10).label("furlong","fur")
  // val inch = (foot / 12.0).label("foot","ft")
  // val yard = (foot * 3).label("yard","yd")
  // val mile = (foot * 5280).label("mile","mi")
  // val league = (mile * 3).label("league","lea")

  // val squareInch = inch.sq
  // val squareFoot = foot.sq
  // val squareChain = chain.sq
  // val acre = (squareFoot * 43560).label("acre","acre")
  // val squareMile = mile.sq
  // val section = squareMile.label("section","section")
  // val township = (section * 36).label("township","twp")

  // val cubicInch = inch.cu
  // val cubicFoot = foot.cu
  // val cubicYard = yard.cu
  // val acreFoot = (acre * foot).label("acre-foot","acre-ft")

  // val psi = UnitM[Pressure]("pounds per square inch","psi",6894.757293168)

  // val mph = UnitM[Speed]("miles per hour","mph",0.44704)
  // val fps = UnitM[Speed]("feet per second","fps",0.3048)
}
class InternationalFoot extends FootBased(0.3048)
class SurveyFoot extends FootBased(0.30480061)

package object volume {
  trait Fluid {
    // val minim = UnitM[Volume]("minim","min",6.1611519921875e-8)
    // val dram = (minim * 60).label("fluid dram","fl dr")
    // val teaspoon = (minim * 80).label("teaspoon","tsp")
    // val tablespoon = (teaspoon * 3).label("tablespoon","Tbsp")
    // val ounce = (tablespoon * 2).label("fluid ounce","fl oz")
    // val shot = (tablespoon * 3).label("shot","jig")
    // val gill = (ounce * 4).label("gill","gi")
    // val cup = (gill * 2).label("cup","cp")
    // val pint = (cup * 2).label("pint","pt")
    // val quart = (pint * 2).label("quart","qt")
    // val gallon = (quart * 4).label("gallon","gal")
    // val barrel = (gallon * 31.5).label("barrel","bbl")
    // val oilBarrel = (gallon * 42).label("oil barrel","bbl")
    // val hogshead = (gallon * 63).label("hogshead","hogshead")
  }
  trait Dry {
    // val pint = UnitM[Volume]("pint","pt",0.0005506104713575)
    // val quart = (pint * 2).label("quart","qt")
    // val gallon = (quart * 4).label("gallon","gal")
    // val peck = (gallon * 2).label("peck","pk")
    // val bushel = (peck * 4).label("bushel","bu")
    // val barrel = UnitM[Volume]("barrel","bbl",0.115627)
  }
}

trait Nautical {
  // val fathom = UnitM[Length]("fathom","ftm",1.8288)
  // val cable = (fathom * 120).label("cable","cb")
  // val nauticalMile = UnitM[Length]("nautical mile","nmi",1852)
  // val knot = UnitM[Speed]("knot","kt",0.514444444)
}

object Mass {
  trait Avoirdupois {
    // val grain = UnitM[Mass]("grain","gr",0.06479891)
    // val dram = (grain * (27 + 11/32)).label("dram","dr")
    // val ounce = (dram * 16).label("ounce","oz")
    // val pound = (ounce * 16).label("pound","lb")
    // val hundredweight = (pound * 100).label("hundredweight","cwt")
    // val longHundredweight = (pound * 112).label("long hundredweight","long cwt")
    // val ton = (hundredweight * 20).label("ton","ton")
    // val longTon = (longHundredweight * 20).label("long ton","long ton")
  }
  trait Troy {
    // val grain = UnitM[Mass]("troy grain","gr",0.06479891)
    // val pennyweight = (grain * 24).label("pennyweight","dwt")
    // val ounce = (pennyweight * 20).label("troy ounce","oz t")
    // val pound = (ounce * 12).label("troy pound","lb t")
  }
}

trait Base {
  // val fahrenheit = UnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)
  // val poundForce = UnitM[Force]("pound force", "lbf",4.4482216152605)
  // val BTU = UnitM[Energy]("British thermal unit","BTU",1055.056)
  // val calorie = UnitM[Energy]("calorie","cal",4.184)
  // val hand = UnitM[Length]("hand","h",0.1016)
  // val lbft = UnitM[Torque]("pound-foot","lb-ft",1.3558179483314004)
  // val bhp = Horsepower.mechanical
  object Horsepower {
    // val mechanical = UnitM[Power]("mechnical horsepower","hp(I)",745.699881448)
    // val electric = UnitM[Power]("electric horsepower","hp(E)",746)
    // val boiler = UnitM[Power]("boiler horsepower","hp(S)",9812.5)
  }
  // val mpg = UnitM[Automotive.DistancePerFuel]("miles per gallon","mpg",425143.707)
}

trait All extends InternationalFoot with Mass.Avoirdupois with Nautical with Base {
  object Survey extends SurveyFoot
  object Fluid extends volume.Fluid
  object Dry extends volume.Dry
  object Troy extends Mass.Troy
}