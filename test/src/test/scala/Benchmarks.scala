package scunits.test

import scunits._
import scunits.default._
import scunits.us._

import org.specs2.mutable._

case class Boxed(v: Double) {
  def +(r: Boxed) = Boxed(v + r.v)
  def -(r: Boxed) = Boxed(v - r.v)
  def /(r: Boxed) = Boxed(v / r.v)
  def *(r: Boxed) = Boxed(v * r.v)

  def +(r: Double) = Boxed(v + r)
  def -(r: Double) = Boxed(v - r)
  def /(r: Double) = Boxed(v / r)
  def *(r: Double) = Boxed(v * r)
}
case class ValueClass(v: Double) extends AnyVal {
  def +(r: ValueClass) = ValueClass(v + r.v)
  def -(r: ValueClass) = ValueClass(v - r.v)
  def /(r: ValueClass) = ValueClass(v / r.v)
  def *(r: ValueClass) = ValueClass(v * r.v)

  def ÷(r: Double) = ValueClass(v / r)
  def ×(r: Double) = ValueClass(v * r)
}
case class ImpValueClass[D <: Dims](v: Double) extends AnyVal {
  def +(r: ImpValueClass[D]) = ImpValueClass[D](v + r.v)
  def -(r: ImpValueClass[D]) = ImpValueClass[D](v - r.v)
  def /[R <: Dims](r: ImpValueClass[R])(implicit m: Mult[D,R]) = ImpValueClass[m.Out](v / r.v)
  def *[R <: Dims](r: ImpValueClass[R])(implicit m: Div[D,R]) = ImpValueClass[m.Out](v * r.v)

  def ÷(r: Double) = ImpValueClass[D](v / r)
  def ×(r: Double) = ImpValueClass[D](v * r)
}
trait Mult[L <: Dims, R <: Dims] {
  type Out <: Dims
}
trait Div[L <: Dims, R <: Dims] {
  type Out <: Dims
}

case class BenchResult[A](name: String, result: A, time: Long, cycles: Long)

class Benchmarks extends Specification {
  sequential

  implicit def mult[L <: Dims, R <: Dims] = new Mult[L,R] {
    type Out = L#mult[R]
  }
  implicit def div[L <: Dims, R <: Dims] = new Div[L,R] {
    type Out = L#div[R]
  }

  def time[A](f: () => A)(name: String, cycles: Long) = {
      val start = System.nanoTime
      val res = f()
      val time = (System.nanoTime - start)
      BenchResult(name, res, time, cycles)
    }

  def BenchArithmetic(cycles: Int) = {
    val doubles = Vector.fill(cycles)(util.Random.nextDouble)
    val measures = doubles.map(Measure[Acceleration](_))
    val boxes = doubles.map(Boxed(_))
    val valueClasses = doubles.map(ValueClass(_))
    val impValueClasses = doubles.map(ImpValueClass[Acceleration](_))

    val dRes = time(() => doubles.foldLeft(0.0) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    })("Double",cycles)

    val mRes = time(() => measures.foldLeft(Measure[Acceleration](0.0)) { (res,el) =>
      res / 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5
    }.v)("Measure",cycles)

    val bRes = time(() => boxes.foldLeft(Boxed(0.0)) { (res,el) =>
      res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5 + res / 2.0 + el * 4.0 - (el + res) * 0.5
    }.v)("Boxed",cycles)

    val vRes = time(() => valueClasses.foldLeft(ValueClass(0.0)) { (res,el) =>
      res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5
    }.v)("ValueClass",cycles)

    val iRes = time(() => impValueClasses.foldLeft(ImpValueClass[Acceleration](0.0)) { (res,el) =>
      res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5 + res ÷ 2.0 + el × 4.0 - (el + res) × 0.5
    }.v)("ImpValueClass",cycles)

    Seq(dRes, mRes, bRes, vRes, iRes)
  }

  def BenchArray(arraySize: Int) = {
    val double = Vector.fill(arraySize)(util.Random.nextDouble).toArray
    val measureA = ArrayM[Length](double)
    val measure = double.map(Measure[Length](_))

    import scala.collection.mutable._

    def iterate[A](a: ArrayLike[A,_], sum: (A,A) => A) = {
      var i = 0;
      while(i < a.length) {

        var ii = i
        while(ii < a.length) {
          a(i) = sum(a(i), a(ii))
          ii += 1
        }

        i += 1
      }
    }

    val mop = (l: Measure[Length], r: Measure[Length]) => l + r

    val cycles = arraySize * arraySize / 2

    Seq(
      time(() => iterate(double, (l: Double, r: Double) => l + r))("Array[Double]",cycles),
      time(() => iterate(measureA, mop))("ArrayM[Measure[Length]]",cycles),
      time(() => iterate(measure, mop))("Array[Measure[Length]]",cycles)
    )
  }

  val preJitRes = BenchArithmetic(10000)
  val jitRes = BenchArithmetic(500000)
  val preJitArray = BenchArray(300)
  val jitArray = BenchArray(4000)

  def printAll(brs: (String,Seq[BenchResult[_]])*) {    
    brs.foreach { brn =>
      val (n,br) = brn
      println()
      println("-- " + n)
      br.foreach { r => println(r.name + ": " + r.time / r.cycles + " ns/cycle") }
    }
  }

  printAll(
    "Pre-JIT Arithmetic"->preJitRes,
    "JITed Arithmetic"->jitRes,
    "Pre-JIT Array"->preJitArray,
    "JITed Array"->jitArray
  )
  println()

  "All tests" should {
    "Produce the same results as Doubles" in {
      Seq(preJitRes, jitRes).map { brs =>
        brs.map(_.result).toSet.size ==== 1
      }
    }
  }

  "Measures" should {
    "Out-perform boxed values before JIT has warmed up" in {
      preJitRes(1).time must be_<(preJitRes(2).time)
    }
    "Perform similarly to Doubles once it has warmed up" in {
      jitRes(1).time must be_<((jitRes(0).time * 1.1).toLong)
    }
  }

  "ArrayM[_]s" should {
    "Out-perform Array[Measure[_]]" in {
      jitArray(1).time must be_<(jitArray(2).time)
    }
    "Perform like Array[Double]" in {
      jitArray(1).time must be_<((jitArray(0).time * 1.1).toLong)
    }
  }
}