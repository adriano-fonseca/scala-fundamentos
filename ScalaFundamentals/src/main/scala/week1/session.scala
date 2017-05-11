package week1

/**
  * Created by adriano-fonseca on 11/11/2016.
  */
object session extends App {

  def sqrt(x: Double) = {

    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) {
        guess
      } else {
        sqrtIter(improve(guess))
      }
    }

    def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) = (guess + (x / guess)) / 2

    sqrtIter(1.0)
  }

  print(sqrt(25))
  //print(sqrt(1e60))
}
