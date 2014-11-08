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
@implicitNotFound(msg = "Cannot find an implicit QList which includes all of the operand's BaseQuantities. You may need to import or create one.")
trait QListOf[+Bs] extends QList {
  type self <: QListOf[Bs]
  type base <: Bs
}
class ::[H <: BaseQuantity, T <: QList] extends QListOf[H#of with T#base] {
  type self = H :: T
  type base = H#of with T#base
  type zeros = T#zeros with H#set[_0]
  type append[As <: QList] = H :: T#append[As]

  protected type doInv[L <: base] = H#setNonZero[T#doInv[L], H#get[L]#neg]

  protected type doPow[L <: base, R <: Integer] = H#setNonZero[T#doPow[L,R], H#get[L]#mult[R]]

  protected type doMult[L <: base, R <: base] = ({
    type exp = H#get[L]#add[H#get[R]]
    type rem = T#doMult[L,R]
    type apply = exp#dimsNzAdd[rem, H]
  })#apply

  protected type doDiv[L <: base, R <: base] = ({
    type exp = H#get[L]#sub[H#get[R]]
    type rem = T#doDiv[L,R]
    type apply = exp#dimsNzAdd[rem, H]
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