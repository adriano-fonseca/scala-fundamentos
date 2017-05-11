package week3

/**
  * Created by adriano-fonseca on 25/11/2016.
  */
trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false

  override def toString: String = return "{ " + this.head + " -> " + "Nil" + " }"
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

object Main extends App {
  val a = singleton[Boolean](true)
  val b = singleton(true)
  println("a => " + a)
  println("b => " + b)

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth[T](n: Int, xs: List[T]): T = {
      if(xs.isEmpty) throw new IndexOutOfBoundsException
      else if (n == 0) xs.head
      else nth(n - 1, xs.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  println(nth(2, list))

}

