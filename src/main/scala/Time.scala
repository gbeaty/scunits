package scunits

import scunits.quantity._

package object time {  
  val second = UnitM[Time.Base]("second","s")
  val minute = UnitM[Time.Base]("minute","min",60.0)
  val hour = UnitM[Time.Base]("hour","hr",3600.0)
}