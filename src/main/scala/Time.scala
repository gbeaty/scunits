package scunits

package object time {
  import scunits.Quantities._
  
  val second = UnitM[Time.Base]("second","s")
  val minute = UnitM[Time.Base]("minute","min",60.0)
  val hour = UnitM[Time.Base]("hour","hr",3600.0)
}