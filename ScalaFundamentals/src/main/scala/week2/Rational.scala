package week2

/**
  * Created by adriano-fonseca on 25/11/2016.
  */

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must to be nonzero")

  val numer = x
  val denom = y

  def this(x: Int) = this(x, 1)

  def max(that: Rational) = if (this < that) that else this

  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def -(that: Rational): Rational = this + -that

  def +(that: Rational): Rational = {
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      that.denom * this.denom
    )
  }

  def unary_- : Rational = new Rational(-numer, denom)

  override def toString: String = {
    val g = gcd(x, y)
    numer / g + "/" + denom / g
  }

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}