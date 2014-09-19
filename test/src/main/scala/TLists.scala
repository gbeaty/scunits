package scunits.types

object DListTests {
  trait A extends scunits2.Quantity
  trait B extends scunits2.Quantity
  trait C extends scunits2.Quantity
  type BC = B :: C :: QNil
  type ABC = A :: BC

  type ZZ = i0 ::: i0 ::: DNil
  type ZZZ = i0 ::: ZZ
  type Z12 = i0 ::: i1 ::: i2 ::: DNil
  type ZZ12 = i0 ::: Z12
  type Two = i2 ::: DNil

  // Test Dimless:
  implicitly[BC#Dimless =:= ZZ]

  // Test Neg:
  implicitly[DNil#Neg =:= DNil]
  implicitly[ZZ#Neg =:= ZZ]
  implicitly[Z12#Neg =:= (i0 ::: i1#Neg ::: i2#Neg ::: DNil)]

  // Test Set:
  implicitly[ZZ#Set[i0,i0] =:= ZZ]
  implicitly[ZZ#Set[i0,i1] =:= (i1 ::: i0 ::: DNil)]
  implicitly[ZZ#Set[i1,i3] =:= (i0 ::: i3 ::: DNil)]
  implicitly[ZZ#Set[i2,i1] =:= ZZ]
  // implicitly[ZZ#Set[i4,i4] =:= (i0 ::: i0 ::: i0 ::: i4 ::: DNil)]

  // Test Mult:
  implicitly[ZZ#Mult[DNil] =:= ZZ]
  implicitly[DNil#Mult[ZZ] =:= ZZ]
  implicitly[ZZ#Mult[ZZ] =:= ZZ]
  implicitly[Z12#Mult[ZZ12] =:= (i0 ::: i1 ::: i3 ::: i2 ::: DNil)]
  implicitly[ZZ#Mult[Z12] =:= Z12]

  // Test Div:
  implicitly[Z12#Div[DNil] =:= Z12]
  implicitly[DNil#Div[ZZ] =:= ZZ]
  implicitly[ZZ#Div[ZZ] =:= ZZ]
  implicitly[Z12#Div[Z12] =:= ZZZ]
  implicitly[Z12#Div[ZZ12] =:= (i0 ::: i1 ::: i1 ::: i2#Neg ::: DNil)]
  implicitly[ZZ#Div[Z12] =:= Z12#Neg]
}