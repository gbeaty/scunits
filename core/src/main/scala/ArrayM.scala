package scunits

import scunits.types._

import scala.reflect.ClassTag
import scala.collection.mutable._

class ArrayMBuilder[D <: Dims] extends Builder[Scalar[D],ArrayM[D]] {
  private val underlying = new ArrayBuilder.ofDouble
  def +=(m: Scalar[D]) = {
    underlying += m.v
    this
  }
  def clear { underlying.clear }
  def result = new ArrayM[D](underlying.result)
}

case class ArrayM[D <: Dims](array: Array[Double])(implicit ct: ClassTag[Scalar[D]]) 
  extends ArrayLike[Scalar[D],ArrayM[D]] {
  
  def apply(index: Int) = Scalar[D](array(index))
  def length = array.length
  def update(idx: Int, elem: Scalar[D]) {
    array(idx) = elem.v
  }

  def seq: IndexedSeq[Scalar[D]] = ???
  protected def newBuilder = new ArrayMBuilder[D]
}