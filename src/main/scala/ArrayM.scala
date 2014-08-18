package scunits

import scala.reflect.ClassTag
import scala.collection.mutable._

class ArrayMBuilder[D <: Dimensions] extends Builder[Measure[D],ArrayM[D]] {
  private val underlying = new ArrayBuilder.ofDouble
  def +=(m: Measure[D]) = {
    underlying += m.v
    this
  }
  def clear { underlying.clear }
  def result = new ArrayM[D](underlying.result)
}

case class ArrayM[D <: Dimensions](array: Array[Double])(implicit ct: ClassTag[Measure[D]]) 
  extends ArrayLike[Measure[D],ArrayM[D]] {
  
  def apply(index: Int) = Measure[D](array(index))
  def length = array.length
  def update(idx: Int, elem: Measure[D]) = array(idx) = elem.v

  def seq: IndexedSeq[Measure[D]] = ???
  protected def newBuilder = new ArrayMBuilder[D]
}