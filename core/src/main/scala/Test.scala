package scunits

import scunits.integer._

object Test {
  type p5 = p4#Succ
  type p6 = p5#Succ
  type p7 = p6#Succ
  type p8 = p7#Succ
  type p9 = p8#Succ

  // Base: 0s

  // val one = Measure[(Mass^p1) :: (Length^p1) :: (Time^p1) :: (Current^p1) :: (Temperature^p1) :: Dimless](1)
  // val one4 = one * one * one * one
  // 3s

  // val four = Measure[(Mass^p4) :: (Length^p4) :: (Time^p4) :: (Current^p4) :: (Temperature^p4) :: Dimless](1)
  // val four4 = four * four * four * four
  // 6s

  // val three7s = Measure[(Mass^p7) :: (Length^p7) :: (Time^p7) :: (Current^p7) :: Dimless](1)
  // val three7sx4 = three7s * three7s * three7s * three7s // 20s, 10s without removing q^0s.
  // val three7sx4Div = three7s / three7s / three7s / three7s // 3s

  // val sev = Measure[(Mass^p7) :: (Length^p7) :: (Time^p7) :: (Current^p7) :: (Temperature^p7) :: Dimless](1)
  // val sev4 = sev * sev * sev * sev
  // 25s

  // val five7s = Measure[(Mass^p7) :: (Length^p7) :: (Time^p7) :: (Current^p7) :: (Temperature^p7) :: (Info^p7) :: Dimless](1)
  // val five7sx4 = five7s * five7s * five7s * five7s
  // 27s

  // val eight = Measure[(Mass^p8) :: (Length^p8) :: (Time^p8) :: (Current^p8) :: (Temperature^p8) :: Dimless](1)
  // val eight4 = eight * eight * eight * eight
  // inf

  // val nine = Measure[(Mass^p9) :: (Length^p9) :: (Time^p9) :: (Current^p9) :: (Temperature^p9) :: Dimless](1)
  // val nine4 = nine * nine * nine * nine
  // inf
}

/*
Non-lambda test: 8s
Lambda test: 8s

Non-lambda test: 10s
Lambda full: 10s

Kept Lambda.

With multSkip full: inf
*/