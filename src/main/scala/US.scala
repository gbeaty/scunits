package scunits

package object us {
  val inch = UnitM[Length.type]("inch","in",0.0254)
  val foot = UnitM[Length.type]("foot","ft",0.3048)
  val yard = UnitM[Length.type]("yard","yd",0.9144)
  val mile = UnitM[Length.type]("mile","mi",1609.34)
}