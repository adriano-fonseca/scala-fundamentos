  val soma = (a: Int, b: Int) => a + b
  val cub = (a: Int) => a * a * a

  def combine(f: (Int, Int) => Int, g: (Int) => Int): (Int, Int) => Int = {
    (x: Int, y: Int) => f(g(x), g(x))
  }

  val fcombinada = combine(soma, cub)
  fcombinada(2, 4)


  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
    /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set =  (x: Int) => x == elem

  val a = singletonSet(1)
  val b = singletonSet(2)
 // contains(a,1)

  val bound = 1000

  /**
    * Prints the contents of a set on the console.
    */

  def union(s: Set, t: Set): Set = (a: Int) => contains(s,a) || contains(t,a)

  val c = union(a,b)

  contains(c,1)
  contains(c,2)
  contains(c,3)

  val even = (x: Int) => (x%2==0)
  even(5)
  val asadas =(1 to 10)

  class Pessoa (val name: String, val age: Int){
    override def toString: String = name +" - "+ age
  }


  val p1 = new Pessoa("Adriano", 30)
  val p2 = new Pessoa("Julia", 6)
  val p3 = new Pessoa("Livia", 4)
  //name.map(char => char.toUpper).groupBy(char => char).map(x => (x._1,x._2.size)).toList.sortBy(pair => pair._2).reverse
  val list = p1 :: p2 :: p3 :: Nil
  list.span(_.age < 29)
  list.filter(p => p.age <= 18)
  list.span(p => p.age <= 18)

