package scunits

package object us {
  import scunits.Quantities._
  
  val inch = UnitM[Length.Base]("inch","in",0.0254)
  val foot = UnitM[Length.Base]("foot","ft",0.3048)
  val yard = UnitM[Length.Base]("yard","yd",0.9144)
  val mile = UnitM[Length.Base]("mile","mi",1609.34)
}