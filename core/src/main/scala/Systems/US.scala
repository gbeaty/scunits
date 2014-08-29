package scunits.system

import scunits._
import scunits.quantity._

package object us {
  val inch = UnitM[Length]("inch","in",0.0254)
  val foot = UnitM[Length]("foot","ft",0.3048)
  val yard = UnitM[Length]("yard","yd",0.9144)
  val mile = UnitM[Length]("mile","mi",1609.34)

  val mph = UnitM[Speed]("miles per hour", "mph", 0.44704)
  val fps = UnitM[Speed]("feet per second", "fps", 0.3048)
}