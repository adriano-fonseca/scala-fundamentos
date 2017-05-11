package week4.ListOperations

/**
  * Created by adriano-fonseca on 29/11/2016.
  */
object CreatingAndFindingElemInList extends App {
  val list = 1 :: 2 :: 3 :: Nil
  val list2 = Nil.::(6).::(5).::(4)
  println("the element in 2 is "+list(2))
  println("the last is "+list.last)
  println("the head is" +list.head)
  println("the list without head is"+list.tail)
  println("the list without last is"+list.init)
  val union = list ++ list2
  println("Union from list and list2 "+ union)
  println("Also Union from list and list2 "+ (list ::: list2))
  println("Union reversed "+ union.reverse)
  println("In Union reversed index of 4 is "+ union.reverse.indexOf(4))
  println("Union has 4 ? "+ union.contains(4)+", but has 9 ? " +union.contains(9))
  println("Taking 3 from union (firsts) "+ union.take(3))
  println("Taking Union less 3 firsts "+ union.drop(3))
  println("Union without 666 in index 2 "+ union.updated(2,666))
}