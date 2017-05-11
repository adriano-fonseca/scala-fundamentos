package week1

/**
  * Created by adriano-fonseca on 11/11/2016.
  */
object factorial extends App{

  def factorial(number: Int) : Int = {
    if(number==0){
      return 1;
    }else{
       number * factorial(number-1)
    }
  }

  def factorialTail(number: Int) : Int = {
    def loop(acc: Int, n: Int) : Int = {
      if (n == 0) {
        acc
      } else {
        loop(acc * n, n - 1)
      }
   }

   loop(1,number)
  }

  println(factorialTail(5))

}
