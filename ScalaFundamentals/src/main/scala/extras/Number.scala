package extras

import week4.Number

/**
  * Created by adriano-fonseca on 29/11/2016.
  */
class NumberT(val n: Int) {
  override def toString: String = n.toString
  def +(op: NumberT) : NumberT = NumberT(n*op.n)
}

object NumberT{
  def apply(n: Int) = new NumberT(n)
}

object main extends App{
  val a = NumberT(3)
  val b = NumberT(4)
  println(a+b)
}

