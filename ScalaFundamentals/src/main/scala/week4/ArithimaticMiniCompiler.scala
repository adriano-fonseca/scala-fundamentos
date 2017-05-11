package week4

import scala.io.StdIn

/**
  * Created by adriano-fonseca on 29/11/2016.
  */


trait Expr{
  def eval : Int = this match {
    case Number(n) => n
    case Sum(e1,e2)  => e1.eval + e2.eval
    case Prod(e1,e2) => e1.eval * e2.eval
    case Div(e1,e2) => e1.eval / e2.eval
    case Var(x) => println(x+" value is?"); StdIn.readInt()
  }
}

case class Div(val e1: Expr, val e2: Expr) extends Expr

case class Sum(val e1: Expr, val e2: Expr) extends Expr

case class Prod(val e1: Expr, val e2: Expr) extends Expr

case class Var(x: String) extends Expr

case class Number(n: Int) extends Expr

object expres{
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1,e2)  => show(e1) + " + " + show(e2)
    case Prod(e1,e2) => show(e1) + " * " + show(e2)
    case Div(e1,e2) => show(e1) + " * " + show(e2)
    case Var(x) => x
  }
}


object arithimeticMiniCompiler extends App{
  val op = Sum(Prod(Number(3),Number(3)) ,Var("x"))
  ///val ex = Sum(Prod(Number(2),Var("x")), Var("y")) // (2 + x) * y
  //println(expres.show(ex))
  println(expres.show(op))
  println(op.eval)
}