package objsets

import TweetReader._


/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p,new Empty)


  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet

    def fewestRetweeted: Tweet

    def fewestRetweetedAccumulator(t: Tweet): Tweet

    def mostRetweetedAccumulator(t: Tweet): Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def descendingByRetweet: TweetList

    def ascendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  def reduce(f: Int) : Int

  def reduceByRetweet : Int = reduce(0)

}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    */
  def union(that: TweetSet): TweetSet = that

  override def toString: String = "."

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  override def mostRetweeted: Tweet = throw new NoSuchElementException("Empty")

  override def mostRetweetedAccumulator(t: Tweet): Tweet = t

  override def fewestRetweeted: Tweet = throw new NoSuchElementException("Empty")

  override def fewestRetweetedAccumulator(t: Tweet): Tweet = t

  override def descendingByRetweet: TweetList = Nil

  override def ascendingByRetweet: TweetList = Nil

  override def reduceByRetweet: Int = reduce(0)
  override def reduce(acc: Int): Int = acc
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    right.filterAcc(p, left.filterAcc(p, {
      if (p(elem)) {
        acc incl elem
      } else {
        acc
      }
    }))
  }

  /**
    * The following methods are already implemented
    */

  def contains(x: Tweet): Boolean =
  if (x.text < elem.text) left.contains(x)
  else if (elem.text < x.text) right.contains(x)
  else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    */
  def union(that: TweetSet): TweetSet = right.union(left.union(that.incl(elem)))

  override def toString: String = "{" + left + elem + right + "}"

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
    def mostRetweeted: Tweet = {
        mostRetweetedAccumulator(elem)
    }

  def fewestRetweeted: Tweet = {
    fewestRetweetedAccumulator(elem)
  }

  def fewestRetweetedAccumulator(t: Tweet): Tweet = right.fewestRetweetedAccumulator(left.fewestRetweetedAccumulator(
    if (t.retweets > elem.retweets) elem else t
  ))

  override def mostRetweetedAccumulator(t: Tweet): Tweet = right.mostRetweetedAccumulator(left.mostRetweetedAccumulator(
    if (t.retweets > elem.retweets) t else elem
  ))

  override def descendingByRetweet: TweetList = {
    val tweet = mostRetweeted
    val setUpdate = this.remove(tweet)
    new Cons(tweet, setUpdate.descendingByRetweet)
  }

  override def ascendingByRetweet: TweetList = {
    val tweet = fewestRetweeted
    val setUpdate = this.remove(tweet)
    new Cons(tweet, setUpdate.ascendingByRetweet)
  }

  override def reduceByRetweet : Int = reduce(0)

  def reduce(acc: Int): Int = right.reduce(left.reduce(
    acc + elem.retweets
  ))
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean


  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
  }

  def reduce : Int = {
    reduce(0+head.retweets,tail)
  }
  def reduce(acc: Int, restOf: TweetList) : Int = {
    reduce(acc+restOf.head.retweets,restOf.tail)
  }

  override  def toString = { head + "->" tail}
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val allTweets = TweetReader.allTweets

  def tweetContaisSomeWordInList(words: List[String], tweet: Tweet) : Boolean = words.exists(word => tweet.text.contains(word))
  def tweetWithoutWordsInList(words: List[String], tweet: Tweet) : Boolean = words.exists(word => !tweet.text.contains(word))

  lazy val googleTweets: TweetSet = allTweets.filter(tweet => tweetContaisSomeWordInList(google,tweet))
  lazy val appleTweets: TweetSet = allTweets.filter(tweet => tweetContaisSomeWordInList(apple,tweet))

  //lazy val googleTweets: TweetSet = allTweets.filter(tweet => tweetWithoutWordsInList(google,tweet))
  //lazy val appleTweets: TweetSet = allTweets.filter(tweet => tweetWithoutWordsInList(apple,tweet))


  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
   lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
  //TweetReader.allTweets.foreach(x => println(x))
  trending.foreach(tweet => println(tweet))
  println(googleTweets.reduceByRetweet)
  }


object Main extends App {
  /*
    val t1 = new Tweet("Adriano 1", "aaa", 30)
    val t2 = new Tweet("Adriano 2", "bbb", 4)
    val t3 = new Tweet("Adriano 3", "aac", 10)
    val t4 = new Tweet("Adriano 2.1", "a", 20)
    val t5 = new Tweet("Adriano 2.3", "ab", 21)
    val list = new Cons(t1,new Cons(t2,new Cons(t3,Nil)))
    println(list.reduce)

  //  var set = new NonEmpty(t1, new Empty, new Empty).incl(t2).incl(t3)
    //val set2 = new NonEmpty(t3,new Empty, new Empty)
   // val set2 = new NonEmpty(t4, new Empty, new Empty).incl(t5).union(set)

    var set = new NonEmpty(t1, new Empty, new Empty)
    var set2 = new NonEmpty(t2, new Empty, new Empty)

    val setUni = set.union(set2)
      setUni.foreach(x => println(x))
    println("reduce: "+setUni.reduceByRetweet)



   val filtered = set2.filter(x => x.retweets >= 20 )
   filtered.foreach(x => println(x))
   //println("most is "+filtered.mostRetweeted.retweets)

     println("")
     println("Ascending")
     filtered.ascendingByRetweet.foreach(x => println(x))

   println("")
   println("Descending")
   filtered.descendingByRetweet.foreach(x => println(x))


     val tweet = filtered.mostRetweeted
     val lista  = new Cons(tweet, Nil)

     println("\n primeira iter ")
     lista.foreach(x => println(x))

     val filtered2 = filtered.remove(tweet)
     val tweet2 = filtered2.mostRetweeted
     val lista2 = new Cons(tweet2, lista)
     println("\nsegunda iter")
     lista2.foreach(x => println(x))

     val filtered3 = filtered2.remove(tweet2)
     val tweet3 = filtered3.mostRetweeted
     val lista3 = new Cons(tweet3, lista2)
     println("\nterceira iter")
     lista3.foreach(x => println(x))

     val filtered4 = filtered3.remove(tweet3)
     val tweet4 = filtered4.mostRetweeted
     val lista4 = new Cons(tweet4, lista3)
     println("\nterceira iter")
     lista4.foreach(x => println(x))

     //set.filter(tweet => tweet.retweets == 10).foreach(x => println(x))
     */
    // Print the trending tweets
    GoogleVsApple

}
