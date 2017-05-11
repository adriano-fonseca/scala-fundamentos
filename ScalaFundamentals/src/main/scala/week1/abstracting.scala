package week1
import scala.io._

/**
  * Created by adriano-fonseca on 28/11/2016.
  */
object Abstracting extends App{

  def readAndCombine(n: Int, combine: (Int, Int) => Int, zero: Int): Int ={
    if (n > 0) {
      println("A number:")
      val grade = StdIn.readInt
      combine(grade,readAndCombine(n-1,combine,zero))
    } else {
      zero
    }
  }

  def sumNumber(n: Int): Int = {
    if (n > 0) {
      println("A number to sum:")
      val grade = StdIn.readInt
      grade + sumNumber(n - 1)
    } else {
      0
    }
  }

  def multNumber(n: Int): Int = {
    if (n > 0) {
      println("A number to mul:")
      val grade = StdIn.readInt
      grade * multNumber(n - 1)
    } else {
      1
    }
  }

  println("How many numbers?")
  val n = StdIn.readInt()
  //println(readAndCombine(n,(x,y) => x*y,1))
//  println(readAndCombine(n,_*_,1))
  //println(readAndCombine(n, _+_ ,0))
  println(readAndCombine(n,(x,y) => x*x+y,0))

}
