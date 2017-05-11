import scala.io.Source

object x {
  val file = Source.fromFile("C:\\Users\\adriano-fonseca\\IdeaProjects\\ScalaFundamentals\\src\\main\\scala\\week6\\linuxwords.txt").mkString
  val words = file.split("\n").map(x => x).toList.filter(word => word.forall(char => char.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] = for ((dig, str) <- mnem; letter <- str) yield letter -> dig
  val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordToCode) withDefaultValue Seq()



  def wordToCode(word: String): String = word.toUpperCase().map(x => charCode(x))
  wordToCode("adriano")

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(Nil)
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number.take(split))
        rest <- encode(number.drop(split))
      } yield word :: rest
    }.toSet
  }

  def translate(number: String): Set[String] = encode(number).map(_.mkString(" "))
  translate("7225247386")
  "adriano".map(x => x).groupBy(x => x)
}








