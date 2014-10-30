package scunits.types

trait EList {
  type set[I <: NonNegInt, To <: NonZeroInt] = setInt[I,To] // Seems to accept To <: NonNegInt ??
  protected type setInt[I <: Integer, To <: Integer] <: EList

  type head <: Integer
  type tail <: EList
  type empty <: Bool

  protected type prependZero[To <: EList] = _0 *: To
  type zeros[I <: Integer, T <: Integer] = I#loop[EList,prependZero,T *: ENil]

  type neg <: EList
  
  type mult[R <: EList] = Op[R,+]
  type div[R <: EList] = Op[R,-]
  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] <: EList
  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] <: EList
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] <: EList
}
trait ENel extends EList {
  type head <: Integer
  type tail <: EList  
}
trait *:[H <: Integer, T <: EList] extends ENel {
  type head = H
  type tail = T
  type empty = False
  type self = head *: tail

  type neg = head#neg *: tail#neg
  type setInt[I <: Integer, To <: Integer] = I#isPos#branch[EList, head *: tail#set[I#pred,To], (To *: tail)]

  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNel[self,O]

  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type rem = L#tail#Op[tail,O]
    type res = O[L#head, head]
    type apply = rem#empty#and[res#isZero]#branch[EList, ENil, res *: rem]
  })#apply

  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = O[_0,head] *: tail#OpNil[O]  
}
trait ENil extends EList {
  type head = _0
  type tail = ENil
  type empty = True
  type setInt[I <: Integer, To <: Integer] = To#isZero#branch[EList,ENil,zeros[I,To]]
  type neg = ENil

  type Op[R <: EList, O[_ <: Integer, _ <: Integer] <: Integer] = R#OpNil[O]
  protected type OpNel[L <: ENel, O[_ <: Integer, _ <: Integer] <: Integer] = L#head *: L#tail
  protected type OpNil[O[_ <: Integer, _ <: Integer] <: Integer] = ENil
}