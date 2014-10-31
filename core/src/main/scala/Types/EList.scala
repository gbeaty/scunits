package scunits.types

trait EList {
  type set[I <: NonNegInt, To <: NonZeroInt] = setInt[I,To] // Seems to accept To <: NonNegInt ??
  protected type setInt[I <: Integer, To <: Integer] <: EList

  type head <: Integer
  type tail <: EList

  protected type prependZero[To <: EList] = _0 *: To
  type zeros[I <: Integer, T <: Integer] = I#loop[EList,prependZero,T *: ENil]

  type neg <: EList

  type mult[Es <: EList] <: EList
  type div[R <: EList] = mult[R#neg]
  type buildTail[E <: Integer] <: EList
}
trait ENel extends EList {
  type head <: Integer
  type tail <: EList  
}
trait *:[H <: Integer, T <: EList] extends ENel {
  type head = H
  type tail = T
  type neg = head#neg *: tail#neg
  type setInt[I <: Integer, To <: Integer] = I#isPos#branch[EList, head *: tail#set[I#pred,To], (To *: tail)]
  type mult[Es <: EList] = Es#tail#mult[tail]#buildTail[head + Es#head]
  type buildTail[E <: Integer] = E *: head *: tail
}
trait ENil extends EList {
  type head = _0
  type tail = ENil
  type setInt[I <: Integer, To <: Integer] = To#isZero#branch[EList,ENil,zeros[I,To]]
  type neg = ENil
  type mult[Es <: EList] = Es
  type buildTail[E <: Integer] = E#isZero#branch[EList, ENil, E *: ENil]
}