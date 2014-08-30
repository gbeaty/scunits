package scunits.unit

import scunits._
import scunits.quantity._
import scunits.unit.time._

package object us {
  val pound = MultUnitM[Mass]("pound", "lb", 0.453592)
  val poundForce = MultUnitM[Force]("pound force", "lbf", 4.44822162)

  val inch = MultUnitM[Length]("inch","in",0.0254)
  val foot = MultUnitM[Length]("foot","ft",0.3048)
  val yard = MultUnitM[Length]("yard","yd",0.9144)
  val mile = MultUnitM[Length]("mile","mi",1609.34)

  val mph = MultUnitM[Speed]("miles per hour", "mph", 0.44704)
  val fps = MultUnitM[Speed]("feet per second", "fps", 0.3048)

  val gallon = MultUnitM[Volume]("gallon", "gal", 0.00378541)

  val fahrenheit = MultOffsetUnitM[Temperature]("fahrenheit","°F", 5.0/9.0, 459.67)

  val mpg = MultUnitM[Automotive.DistancePerFuel]("miles per gallon", "mpg", 425142.851104636)
  val squareInch = MultUnitM[Area]("square inch", "sq in", 0.00064516)

  val psi = MultUnitM[Pressure]("pounds per square inch", "psi", 6894.75729)
}