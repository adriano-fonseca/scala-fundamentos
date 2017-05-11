//func -> parametros -> retorno
def sum(f: Int => Int) : (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sumF(a + 1, b)
  sumF
}
sum(f => f)(3,5) //3+4+5


//func -> parametros -> retorno
def sum1(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum1(f)(a + 1, b)
}
println("soma da soma")

sum1(sum => sum + sum)(3,5) //3+4+5 = 12
sum(f => f + f)(3,5) //3+4+5 = 12



def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) : Int = {
  if(a > b) zero
  else
    combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}
mapReduce(f => f, (x,y) => x + y, 0)(3,5)
mapReduce(f => f, (x,y) => x * y, 1)(3,5)

def product(f: Int=> Int)(a: Int, b: Int) : Int = mapReduce(f, (a,b) => a*b, 1)(a,b)

product(f => f)(2,4)

def factorial(f: Int => Int)(n: Int): Int = {
  product(f => f)(1,n)
}

factorial(f => f)(5)

