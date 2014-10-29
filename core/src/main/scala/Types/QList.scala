package scunits.types

import scunits._
import annotation.implicitNotFound

trait QList {
  type base <: Dim
  type op[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] <: base
  type inv[L <: base] <: base
  type zeros <: base
  type append[As <: QList] <: QList
  type Dimless = DimsConst[this.type, zeros]  
  type copy[D <: base] <: base

  type ^[L <: BaseQuantity, R <: NonZeroInt] = DimsConst[this.type, copy[zeros with L#set[R]]]
}
trait QListOf[B <: Dim] extends QList {
  type base = B
}
class ::[L <: BaseQuantity, R <: QList] extends QListOf[L#of with R#base] {
  type head = L
  type tail = R
  type zeros = head#set[_0] with tail#zeros
  type append[As <: QList] = head :: tail#append[As]
  type copy[D <: base] = tail#copy[D] with head#set[head#get[D]]

  type inv[L <: base] = tail#inv[L] with head#set[head#get[L]#neg]
  
  type op[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] =
    tail#op[L,R,O] with head#set[O[head#get[L], head#get[R]]]
}
class QNil extends QListOf[Dim] {
  type zeros = Dim
  type append[As <: QList] = As
  type copy[D <: base] = Dim

  type inv[L <: base] = Dim
  type op[L <: base, R <: base, O[_ <: Integer, _ <: Integer] <: Integer] = Dim
}