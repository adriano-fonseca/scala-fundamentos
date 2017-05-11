object mergeSort {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple","pineapple","orange","banana")


  def sum(xs: List[Int]) = (0 :: xs) reduceLeft(_ + _)
  def product(xs: List[Int]) = (1 :: xs) reduceLeft((x, y) => x * y)

  sum(nums)
  sum(Nil)
  product(List(1,2,3))
  product(List())

  def sum2(xs: List[Int]) = (xs foldLeft 0)(_ + _)
  def product2(xs: List[Int]) = (xs foldLeft 1)(_ * _)

  sum2(nums)
  sum2(Nil)
  product2(List(1,2,3))
  product2(List())

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( f(_)::_ )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (x,y) => 1 + y)


 }
