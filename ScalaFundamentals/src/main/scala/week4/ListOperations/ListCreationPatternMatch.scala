package week4.ListOperations

/**
  * Created by adriano-fonseca on 29/11/2016.
  */
object ListCreationPatternMatch extends App {
  val apenasHeadCaldaNil = "banana" :: Nil
  val semPadraoConhecido = List("a","orange", "apple", "strawberrys")
  val orangeAppleCalda = List("orange", "apple", "strawberrys")
  val fruits = "banana" :: ("apple" :: ("strawberrys" :: Nil))
  val sameFruits = "banana" :: "apple" :: "strawberrys" :: fruits
  val sameFruits2 = Nil.::("strawberry").::("apple").::("banana")

  val num = List("banana", "apple", "strowberrys")
  val diag = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  val empty = List()

  //  listString.foreach(item => println(item))
  // fruits.foreach(item => println(item))
  // sameFruits.foreach(item => println(item))
  //sameFruits2.foreach(item => println(item))

  def defineList(l: List[String]) = {
    if (l == "banana" :: Nil) {
      println("banana tail void")
    } else if (l == Nil || l == List()) {
      println("void")
    } else if(l match {
      case "banana" :: xs => true
      case x :: xs => false
      case "orange" :: "apple":: xs => false
     }
    ){
      println("head: banana e calda")
    } else if (l match {
      case "banana" :: xs => false
      case "orange" :: "apple":: xs => false
      case x :: xs => true
    }
    ){
      println("head e calda (Padrão nao reconhecido)")
    } else if (l match {
      case "orange" :: "apple":: xs => true
      case x :: xs => false
      case "banana" :: xs => false
    }
    ){
      println("head: orange, head:apple e calda")
    }

  }

  defineList(empty)
  defineList(apenasHeadCaldaNil)
  defineList(orangeAppleCalda)
  defineList(fruits)
  defineList(semPadraoConhecido)
}