package scunits.unit

import scunits._
import scunits.quantity._

package object time {  
  val second = BaseUnitM[Time]("second","s")
  val minute = MultUnitM[Time]("minute","min",60.0)
  val hour = MultUnitM[Time]("hour","hr",3600.0)
}