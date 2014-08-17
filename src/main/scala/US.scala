package scunits

package object us {
  import scunits.quantity.Physical._
  
  val inch = UnitM[Length]("inch","in",0.0254)
  val foot = UnitM[Length]("foot","ft",0.3048)
  val yard = UnitM[Length]("yard","yd",0.9144)
  val mile = UnitM[Length]("mile","mi",1609.34)
}