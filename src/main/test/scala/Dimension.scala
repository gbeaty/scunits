package scunits

object DimensionTests {
  import scunits.quantity._
  import Electric._
  import Magnetic._

  implicitly[Current.Base#Add[Time.Dim] =:= Charge]
  implicitly[Time.Base#Add[Current.Dim] =:= Charge]

  implicitly[Volume#Neg =:= DNel[Dimension[_0,_3#Neg],DNil]]

  implicitly[DNil * DNil =:= DNil]
  implicitly[DNil * Length.Base =:= Length.Base]
  implicitly[Length.Base * DNil =:= Length.Base]
  implicitly[Area =:= DNel[Dimension[_0,_2],DNil]]
  implicitly[Volume =:= DNel[Dimension[_0,_3],DNil]]
  implicitly[Charge =:= DNel[Dimension[_1,_1],DNel[Dimension[_8,_1],DNil]]]
  implicitly[Time.Base * Current.Base =:= Charge]

  implicitly[(DNil / Time.Base) =:= DNel[Dimension[_1,_1#Neg],DNil]]
  implicitly[Speed =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_1#Neg],DNil]]]
  implicitly[Acceleration =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNil]]]
  implicitly[Acceleration * Mass.Base =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]]]
  // val a: Speed * Mass.Base = 1
  // val b: Mass.Base * Speed = 1
  // implicitly[(Speed * Mass.Base) =:= (Mass.Base * Speed)]

  // Force =:= DNel[Dimension[_0,_1],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_1],DNil]]].
  // implicitly[(Pressure / DNil) =:= Pressure]
  // val t: Energy / Volume = 1
  // DNel[Dimension[_0,_2#Neg],DNel[Dimension[_1,_2#Neg],DNel[Dimension[_2,_2],DNil]]]
  // implicitly[Energy / Volume =:= Pressure]
}