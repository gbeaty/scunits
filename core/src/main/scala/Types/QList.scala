package scunits.types

import scunits._
import annotation.implicitNotFound

trait QList {
  type self <: QList
  type base <: Dim   

  type mult[L <: Dims, R <: Dims] = self#op[L#values,R#values,+]
  type div[L <: Dims, R <: Dims] = self#op[L#values,R#values,-]
  protected type op[L <: Dim, R <: Dim, O[_ <: Integer, _ <: Integer] <: Integer] = doOp[zeros with L, zeros with R, O]
  protected type doOp[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] <: Dims
  
  type zeros <: base
  type append[As <: QList] <: QList

  type inv[L <: Dims] = DimsConst[L#bases, self#neg[self#zeros with L#values]]
  protected type neg[L <: base] <: Dim

  type pow[L <: Dims, R <: Integer] = DimsConst[L#bases, doPow[zeros with L#values, R]]
  protected type doPow[L <: base, R <: Integer] <: Dim

  object ops {
    type *[L <: Dims, R <: Dims] = mult[L,R]
    type /[L <: Dims, R <: Dims] = div[L,R]
  }
}
@implicitNotFound(msg = "Cannot find an implicit QList for BaseQuantities ${Bs}.")
trait QListOf[+Bs <: Dim] extends QList {
  type self <: QListOf[Bs]
  type base <: Bs
}
class ::[L <: BaseQuantity, R <: QList] extends QListOf[L#of with R#base] {
  type self = L :: R
  type head = L
  type tail = R
  type base = L#of with R#base
  type zeros = head#setDim[tail#zeros, _0]
  type append[As <: QList] = head :: tail#append[As]

  protected type neg[L <: base] = head#setNonZero[tail#neg[L], head#get[L]#neg]
  
  protected type doOp[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type rem = tail#doOp[L,R,O]
    type exp = O[head#get[L], head#get[R]]
    // type apply = exp#isZero#branch[Dims, rem, rem#set[head, exp]]
    // type apply = exp#ifZero[Dims, rem, ({type nz[I <: NonZeroInt] = rem#set[head, I]})#nz]
    type apply = exp#dimsNzAdd[rem, head]
  })#apply

  protected type doPow[L <: base, R <: Integer] = head#setNonZero[tail#doPow[L,R], head#get[L]#mult[R]]

}
trait QNil extends QList {
  type self = QNil
  type base = Dim
  type zeros = Dim
  type append[As <: QList] = As

  protected type neg[L <: base] = Dim
  protected type doOp[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] = Dimless
  protected type doPow[L <: base, R <: Integer] = Dim
}