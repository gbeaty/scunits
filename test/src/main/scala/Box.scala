package scunits.types

object BoxTests {
  type empty = Empty[Integer]
  type b1 = Full[Integer, p1] // Full[Integer, p1]
  type b2 = Full[Integer, p2]
  type ++[I <: Integer] = I#succ
  type boxSucc[I <: Integer] = Full[Integer, I#succ]

  // constructors:
  implicitly[IntBox.box =:= Box[Integer]]
  implicitly[IntBox.full[p2] =:= b2]
  implicitly[IntBox.empty =:= empty]

  // self:
  implicitly[empty#self =:= empty]
  implicitly[b1#self =:= b1]

  // map:
  implicitly[empty#map[++] =:= empty]
  implicitly[b1#map[++] =:= b2]

  // flatMap:
  implicitly[empty#flatMap[boxSucc] =:= empty]
  implicitly[b1#flatMap[boxSucc] =:= b2]

  // getOrElse:
  implicitly[empty#getOrElse[p2] =:= p2]
  implicitly[b1#getOrElse[p2] =:= p1]
}