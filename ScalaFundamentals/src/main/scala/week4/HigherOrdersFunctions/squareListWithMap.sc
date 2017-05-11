/**
  * Created by adriano-fonseca on 07/12/2016.
  */
object mergeSort {
  def  squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case x :: xs => x*x :: squareList(xs)
  }



  val xs = List(1,2,3,5)
  println(squareList(xs))
  println(xs.map(x => x*x))
  val name = "aaabbcccccceeeeeeee"


}
