package scunits.types

import scunits._
import TestTypes._

object ConverterTests {
  def index[A <: QList, Q <: Quantity, I <: NonNegInt](implicit i: QuantIndex[A,Q] { type Index = I }) = i
  def has[A <: QList, Q <: Quantity](implicit i: QuantIndex[A,Q]) = i

  // val c = converter[ABn,ABCn]

  index[ABCDn,A,i0]

  val a = has[ABCDn,A]
  val b = has[ABCDn,B]
  val ai: a.Index = new i0
  val bi: b.Index = new i1

  // index[ABCDn,B,i1]
  /*type BIndexConst = QuantIndexConst[ABCDn,TestTypes.B,i1]
  type BIndex = QuantIndex[ABCDn,TestTypes.B]
  def genB(implicit b: BIndex) = b
  val b: BIndex = indexSearch[ABCDn,TestTypes.B]
  val b2: BIndex = genB
  val i: b.Index = new i1*/


  // index[ABCDn,C,i2]
  // index[ABCDn,D,i3]
  // has[ABCDn,Z]

  // val i = has[ABCDn,B]
  // val a: i.Index = new i1

  // index[ABCDn,B,i1](indexSearch[ABCDn,B])
}