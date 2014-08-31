package scunits.unit

import scunits._
import scunits.quantity._
import scunits.unit.time._

package object us {
  val pound = UnitM[Mass]("pound", "lb", 0.453592)
  val poundForce = UnitM[Force]("pound force", "lbf", 4.44822162)

  val inch = UnitM[Length]("inch","in",0.0254)
  val foot = UnitM[Length]("foot","ft",0.3048)
  val yard = UnitM[Length]("yard","yd",0.9144)
  val mile = UnitM[Length]("mile","mi",1609.34)

  val mph = UnitM[Speed]("miles per hour", "mph", 0.44704)
  val fps = UnitM[Speed]("feet per second", "fps", 0.3048)

  val gallon = UnitM[Volume]("gallon", "gal", 0.00378541)

  val fahrenheit = UnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)

  val mpg = UnitM[Automotive.DistancePerFuel]("miles per gallon", "mpg", 425142.851104636)
  val squareInch = UnitM[Area]("square inch", "sq in", 0.00064516)

  val psi = UnitM[Pressure]("pounds per square inch", "psi", 6894.75729)
}