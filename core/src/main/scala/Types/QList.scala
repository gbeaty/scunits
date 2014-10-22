package scunits.types

trait QList {
  type self <: QList
  type set <: BaseQuantity
  type quant <: Quant   

  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] <: Quant
  
  type zeros <: quant
  type append[As <: QList] <: QList

  type op[L <: Quant, R <: Quant, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type zeros = self#zeros
    type res = self#bugOp[zeros with L, zeros with R, O]
  })#res
  type mult[L <: Quant, R <: Quant] = op[L,R,+]
  type div[L <: Quant, R <: Quant] = op[L,R,-]

  type neg[L <: Quant] = doNeg[zeros with L]
  protected type doNeg[L <: quant] <: Quant
}
trait QListOf[+Q <: Quant] extends QList {
  type quant <: Q
}
class ::[L <: BaseQuantity, R <: QList] extends QListOf[L#of with R#quant] {
  type head = L
  type tail = R
  type quant = L#of with R#quant
  type self = head :: tail
  type set = head with tail#set
  type zeros = head#set[tail#zeros, _0]
  type append[As <: QList] = head :: tail#append[As]

  protected type doNeg[L <: quant] = head#setNonZero[tail#doNeg[L], head#get[L]#Neg]

  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] =
    head#setNonZero[tail#bugOp[L,R,O], O[head#get[L], head#get[R]]]
}
trait QNil extends QList {
  type self = QNil
  type set = BaseQuantity
  type quant = Quant
  type zeros = Quant
  type append[As <: QList] = As

  protected type doNeg[L <: quant] = Quant

  protected type bugOp[L <: quant, R <: quant, O[_ <: Integer, _ <: Integer] <: Integer] = Quant
}