package week2

import week2.{Rational}

object rationalOps extends App {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)

  println(x + "+" + y + "=" + (x + y))
}
