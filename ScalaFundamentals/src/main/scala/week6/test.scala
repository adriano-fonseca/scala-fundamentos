package week6

import scala.io.Source

/**
  * Created by adriano-fonseca on 12/12/2016.
  */
object test extends App{
  val in = Source.fromFile("C:\\Users\\adriano-fonseca\\IdeaProjects\\ScalaFundamentals\\src\\main\\scala\\week6\\linuxwords.txt").mkString
  in.foreach(x=>print(x))
}
