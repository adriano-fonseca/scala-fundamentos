package extras

/**
  * Created by adriano-fonseca on 29/11/2016.
  */
object Intervalos extends App {

  def printIntervalRecursive(start: Int, limite: Int): Unit = {
    println(limite)
    if (limite > 1 && limite != start) printIntervalRecursive(start,limite-1)
  }

  def printIntervalReverseRecursive(start: Int, limite: Int): Unit = {
    println(start)
    if (start <  limite+1 && limite != start) printIntervalReverseRecursive(start+1,limite)
  }




  def printInterval(start: Int, limite: Int, step: Int): Unit = {
    val a =  Range.inclusive(start,limite,step).reverse            //(1 to 10)
    for (i <- a){
      println(i)
    }
    Range.inclusive(1,10,2)
  }

 // printIntervalReverseRecursive(90,100)
  //printInterval(1,100,1)


  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) : Int = {
    if(a > b) zero
    else
      combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  print(List(1,2).reduce((x,y) => x+y))
  //mapReduce(f => f, (x,y) => x + y, 0)(3,5)
  //mapReduce(f => f, (x,y) => x * y, 1)(3,5)

}

