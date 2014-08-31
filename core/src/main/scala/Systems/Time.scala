package scunits.unit

import scunits._
import scunits.quantity._

package object time {  
  val second = UnitM[Time]("second","s")
  val minute = UnitM[Time]("minute","min",60.0)
  val hour = UnitM[Time]("hour","hr",3600.0)
}