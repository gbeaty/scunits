package scunits.types

trait QList {
  type self <: QList
  type set <: BaseQuantity
  type dims <: Dims   

  protected type bugOp[L <: dims, R <: dims, O[_ <: Integer, _ <: Integer] <: Integer] <: Dims
  
  type zeros <: dims
  type append[As <: QList] <: QList

  type op[L <: Dims, R <: Dims, O[_ <: Integer, _ <: Integer] <: Integer] = ({
    type zeros = self#zeros
    type res = self#bugOp[zeros with L, zeros with R, O]
  })#res
  type mult[L <: Dims, R <: Dims] = op[L,R,+]
  type div[L <: Dims, R <: Dims] = op[L,R,-]

  type neg[L <: Dims] = doNeg[zeros with L]
  protected type doNeg[L <: dims] <: Dims

  object ops {
    type *[L <: Dims, R <: Dims] = mult[L,R]
    type /[L <: Dims, R <: Dims] = div[L,R]
  }
}
trait QListOf[+D <: Dims] extends QList {
  type dims <: D
}
class ::[L <: BaseQuantity, R <: QList] extends QListOf[L#of with R#dims] {
  type head = L
  type tail = R
  type dims = L#of with R#dims
  type self = head :: tail
  type set = head with tail#set
  type zeros = head#setDim[tail#zeros, _0]
  type append[As <: QList] = head :: tail#append[As]

  protected type doNeg[L <: dims] = head#setNonZero[tail#doNeg[L], head#get[L]#neg]

  protected type bugOp[L <: dims, R <: dims, O[_ <: Integer, _ <: Integer] <: Integer] =
    head#setNonZero[tail#bugOp[L,R,O], O[head#get[L], head#get[R]]]
}
trait QNil extends QList {
  type self = QNil
  type set = BaseQuantity
  type dims = Dims
  type zeros = Dims
  type append[As <: QList] = As

  protected type doNeg[L <: dims] = Dims

  protected type bugOp[L <: dims, R <: dims, O[_ <: Integer, _ <: Integer] <: Integer] = Dims
}