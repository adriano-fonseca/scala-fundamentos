package week4

/**
  * Created by adriano-fonseca on 30/11/2016.
  */
class ListSort {

}

object test extends App{

  def isort(xs: List[Int]) : List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = xs match{
      case Nil => x :: Nil
      case y :: ys => if(x <= y) x :: xs else y :: insert(x,ys)
    }

    xs match {
      case List() => Nil
      case y :: ys => insert(y, isort(ys))
    }
  }

  val a  = List(8,5,6,1,10)
  println("ordered: "+isort(a))
}
