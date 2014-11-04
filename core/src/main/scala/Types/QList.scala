package scunits.types

import scunits._
import annotation.implicitNotFound

trait QList {
  type self <: QList
  type base

  type mult[L <: Dims, R <: Dims] = self#prepMult[L, R]
  type div[L <: Dims, R <: Dims] = self#prepDiv[L, R]
  protected type prepMult[L <: Dims, R <: Dims] = doMult[zeros with L#values, zeros with R#values]
  protected type prepDiv[L <: Dims, R <: Dims] = doDiv[zeros with L#values, zeros with R#values]
  protected type doMult[L <: base, R <: base] <: Dims
  protected type doDiv[L <: base, R <: base] <: Dims 
  
  type zeros <: base
  type append[As <: QList] <: QList

  type inv[L <: Dims] = DimsConst[L#bases, doInv[zeros with L#values]]
  protected type doInv[L <: base]

  type pow[L <: Dims, R <: Integer] = DimsConst[L#bases, doPow[zeros with L#values, R]]
  protected type doPow[L <: base, R <: Integer]

  object ops {
    type *[L <: Dims, R <: Dims] = mult[L,R]
    type /[L <: Dims, R <: Dims] = div[L,R]
  }

  // Experimental:  
}
@implicitNotFound(msg = "Cannot find an implicit QList for BaseQuantities ${Bs}.")
trait QListOf[+Bs] extends QList {
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

  protected type doInv[L <: base] = head#setNonZero[tail#doInv[L], head#get[L]#neg]

  protected type doPow[L <: base, R <: Integer] = head#setNonZero[tail#doPow[L,R], head#get[L]#mult[R]]

  protected type doMult[L <: base, R <: base] = ({
    type rem = tail#doMult[L,R]
    type exp = head#get[L] + head#get[R]
    type apply = exp#dimsNzAdd[rem, head]
  })#apply

  protected type doDiv[L <: base, R <: base] = ({
    type rem = tail#doDiv[L,R]
    type exp = head#get[L] - head#get[R]
    type apply = exp#dimsNzAdd[rem, head]
  })#apply

  // Experimental:
}
trait QNil extends QList {
  type self = QNil
  type base = Any
  type zeros = Any
  type append[As <: QList] = As

  protected type doInv[L <: base] = Any
  protected type doMult[L <: base, R <: base] = Dimless
  protected type doDiv[L <: base, R <: base] = Dimless
  protected type doPow[L <: base, R <: Integer] = Any

  // Experimental:
}