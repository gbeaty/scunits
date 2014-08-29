package scunits

import scunits.integer._
import scunits.integer.Ops._

object Tests {
  import scunits.quantity._
  import Electric._
  import Magnetic._

  // Test DNil
  implicitly[DNil#Mult[DNil] =:= DNil]  
  implicitly[Length#Mult[DNil] =:= Length]
  implicitly[DNil#Mult[Length] =:= Length]
  implicitly[Length#Div[DNil] =:= Length]
  implicitly[DNil#Div[Length] =:= Length#Neg]

  // Test simple division
  implicitly[Area#Div[Length] =:= Length]
  implicitly[Area#Div[Area] =:= DNil]
  implicitly[Inductance#Div[Inductance] =:= DNil]
  implicitly[Force#Div[Acceleration] =:= Mass]

  // Test associativity
  implicitly[Area#Mult[Length] =:= Length#Mult[Area]]
  implicitly[Flux#Mult[Power] =:= Power#Mult[Flux]]
  // implicitly[Speed#Mult[Mass] =:= Mass#Mult[Speed]]

  // Test other relationships between quantities
  implicitly[Power#Mult[Time] =:= Energy]
  implicitly[Energy#Div[Time] =:= Power]
  implicitly[Acceleration#Mult[Time] =:= Speed]
}