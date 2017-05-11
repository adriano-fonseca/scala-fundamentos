package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    * *
    * val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordToCode) withDefaultValue Seq()
    * def wordToCode(word: String): String = word.toUpperCase().map(x => charCode(x))
    */

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences).withDefaultValue(List())
  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts a sentence into its character occurrence list.
  def sentenceOccurrences(s: Sentence): Occurrences = s match {
    case Nil => List()
    case x :: Nil => wordOccurrences(x)
    case x :: xs => wordOccurrences(x) ++ sentenceOccurrences(xs)
  }  */
  def sentenceOccurrences(s: Sentence): Occurrences = {
      wordOccurrences(s.flatten.mkString)
    }
  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(word: String): Occurrences = word.toLowerCase.groupBy(x => x).mapValues(x => x.length).toList.sorted
                                                 //word.toList.map(x => times(x, word)).map(f => (f._1, f._2)).toList.distinct.sortBy(_._1)

  def times(letter: Char, w: Word): (Char, Int) = (letter.toLower, w.toUpperCase().filter(p => p.equals(letter.toUpper)).size)

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty)
      List(List())
    else {
      for {
        occNum <- 1 to occurrences.size
        occ <- occurrences.take(occNum)
        times <- 1 to occ._2
        rest <- combinations(occurrences.drop(occNum))
      } yield (occ._1, times) :: rest
    }.toList ++ List(List())
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val (intersection, diff) = x.partition(xmember => y.exists(ymember => xmember._1 == ymember._1))

    val a = for {
      xoc <- x
      yoc <- y
      if yoc._1 == xoc._1
    } yield (xoc._1, (xoc._2 - yoc._2)) :: Nil
    (a.flatten ++ diff).filter(x => x._2 > 0)
  }


  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my), -
    * List(en, my, as), -
    * List(man, yes),
    * List(men, say),    -
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def setenceAnagransAcc(occurrences: Occurrences): List[Sentence] ={
        if(occurrences.isEmpty) List(List())
        else{
              val comb = combinations(occurrences) //List[(Char, Int)]
          for{
            i <- comb if dictionaryByOccurrences.keySet(i);
                       j <- dictionaryByOccurrences(i);
                       s <- setenceAnagransAcc(subtract(occurrences, i))
          } yield {j :: s}


        }
    }
    setenceAnagransAcc(sentenceOccurrences(sentence)).distinct
  }

  def printCombSentende(sentence: Sentence): Unit = {
   combinations(sentenceOccurrences(sentence)).foreach(println(_))
  }

}

object main extends App {
  val occurrences2 = List(('l', 2), ('a', 2), ('r', 2),('d', 2) )
  val occurrences = List(('l', 1), ('a', 1), ('r', 1) )

  //val occurrences1 = List(('r', 1))
  val sent = List("Yes","man")
  /*
  val ocs : List[Anagrams.Occurrences] = list.map( x => (for(i <- 1 to (x._2)) yield (x._1,i)).toList)
  println(ocs)
  val res = ocs.foldRight(List[Anagrams.Occurrences](Nil))((x,y) => y ++ (for(i <- x; j <- y) yield (i :: j)))
  println(res)*/
  //val list = Anagrams.combinations(occurrences)
  //list.foreach(println(_))
  //println(Anagrams.subtract(occurrences2, occurrences))
 Anagrams.sentenceAnagrams(sent).foreach(println(_))
 // Anagrams.printCombSentende(sent)

}