package scunits

package object time {
  import scunits.quantity.Physical._
  
  val second = UnitM[Time]("second","s")
  val minute = UnitM[Time]("minute","min",60.0)
  val hour = UnitM[Time]("hour","hr",3600.0)
}