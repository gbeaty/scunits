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

  // val a: Time#Mult[Mass] = 1
  // [error]  required: scunits.quantity.Mass#MultL[scunits.DNel[scunits.quantity.Time.type,scunits.integer.SuccInt[scunits.integer._0],scunits.DNil]]
  // [error]     (which expands to)  scunits.DNel[scunits.quantity.Time.type,scunits.integer.SuccInt[scunits.integer._0],scunits.DNel[scunits.quantity.Mass.type,scunits.integer.SuccInt[scunits.integer._0],scunits.DNil]]
  // This simplifies to: DNel[Time.type,_1,DNel[Mass.type,_1,DNil]]
  
  // val b: Mass#Mult[Time] = 1
  // [error]  required: scunits.quantity.Time#MultL[scunits.DNel[scunits.quantity.Mass.type,scunits.integer.SuccInt[scunits.integer._0],scunits.DNil]]
  // [error]     (which expands to)  scunits.DNel[scunits.quantity.Time.type,scunits.integer.SuccInt[scunits.integer._0],scunits.DNel[scunits.quantity.Mass.type, scunits.integer.SuccInt[scunits.integer._0], scunits.DNil]]
  // This simplifies to: DNel[Time.type,_1,DNel[Mass.type, _1, DNil]]

  implicitly[Time#Mult[Mass] =:= Mass#Mult[Time]]

  // Test other relationships between quantities
  implicitly[Power#Mult[Time] =:= Energy]
  implicitly[Energy#Div[Time] =:= Power]
  implicitly[Acceleration#Mult[Time] =:= Speed]
}