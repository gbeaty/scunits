package scunits

import scunits.integer._

object CompilationTimes {
  def m[D <: Dims] = Measure[D](1)
  def u[D <: Dims] = UnitM[D]("","",1)

  type PlankPower = ::[^[Length,SuccInt[SuccInt[_0]]],::[^[Time,PredInt[_0]],::[^[Time,PredInt[_0]],::[^[Mass,PredInt[_0]],::[^[Time,PredInt[_0]],Dimless]]]]]
  type PlankArea = ::[^[Length,SuccInt[SuccInt[_0]]],Dimless]

  val intensity = m[PlankPower] / m[PlankArea]
  // Way too damn slow:
  // intensity * intensity * intensity * intensity * intensity * intensity * intensity * intensity
}