package scunits.unit

import scala.math.BigDecimal

import scunits._
import scunits.quantity._
import scunits.unit.time._

package object us {
  val pound = UnitM[Mass]("pound", "lb", 0.45359237)
  val poundForce = UnitM[Force]("pound force", "lbf", 4.4482216152605)
  
  // International:
  val point = UnitM[Length]("point","p",0.000352777778)
  val pica = (point * 12).label("pica","P")
  val inch = (pica * 6).label("inch","in")
  val foot = (inch * 12).label("foot","ft")
  val yard = (foot * 3).label("yard","yd")
  val mile = (foot * 5280).label("mile","mi")

  object survey {
    val link = (inch * 7.92).label("link","li")
    val surveyFoot = UnitM[Length]("survey foot","ft",0.30480061)
    val rod = (link * 25).label("rod","rd")
    val chain = (rod * 4).label("chain","ch")
    val furlong = (chain * 10).label("furlong","fur")
    val mile = (furlong * 8).label("survey mile", "mi")
    val league = (mile * 3).label("league","lea")
  }  

  object nautical {
    val fathom = (yard * 2).label("fathom","ftm")
    val cable = (fathom * 120).label("cable","cb")
    val mile = (us.mile * 1.151).label("nautical mile","nmi")
  }  

  val mph = (mile / hour).label("miles per hour","mph")
  val fps = (foot / second).label("feet per second","fps")

  val gallon = UnitM[Volume]("gallon", "gal", 0.00378541178)

  val fahrenheit = UnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)

  val mpg = (mile / gallon).label("miles per gallon","mpg")
  val squareInch = (inch * inch).label("square inch", "sq in")

  val psi = (poundForce / squareInch).label("pounds per square inch","psi")
}