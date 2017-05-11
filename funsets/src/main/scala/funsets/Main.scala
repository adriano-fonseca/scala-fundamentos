package funsets

object Main extends App {
  import FunSets._
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)
  var set2 = union(s4, s2)
  val f = (x: Int) => (x%2==0)
  println(forall(set2,f)==true)  // 3 4, 3), "diff set 1 and set 2, has not 3")
}
