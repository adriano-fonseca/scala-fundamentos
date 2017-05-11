/**
  * Created by adriano-fonseca on 07/12/2016.
  */
object mergeSort {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple","pineapple","orange","banana")

  nums.filter(x => x > 0)
  nums.filterNot(x => x > 0)
  val pair= nums.partition(x => x > 0)

  val(moreThanZero, lessThanZero) = pair
  moreThanZero
  lessThanZero

  println(nums.takeWhile(x => x > 0))
  println(nums.dropWhile(x => x > 0))
  println(nums.span(x => x > 0))


  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs.span( y => y == x)
      first :: pack(rest)
  }


  def encode[T](xs: List[T]) : List[(T, Int)] ={
    pack(xs).map(l => (l.head, l.length))
  }

  //List(List(a, a, a), List(b), List(c, c), List(a))
  encode(List("a", "a", "a", "b", "c", "c", "a"))


}
