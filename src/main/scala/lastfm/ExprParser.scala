package lastfm

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed abstract class Expr {
  def eval():Double
}

case class EConst(value:Double) extends Expr {
  def eval():Double = value
}

case class EAdd(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval + right.eval
}

case class ESum(a:List[Expr]) extends Expr {
  def eval(): Double = a.foldLeft(0.0)(_ + _.eval)
}

case class ESub(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval - right.eval
}

case class EMul(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval * right.eval
}

case class EDiv(left:Expr, right:Expr) extends Expr {
  def eval():Double = left.eval / right.eval
}

/**
  * Created by erodriguez on 07/01/17.
  */
object ExprParser extends StandardTokenParsers {
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")

  def value = numericLit ^^ { s => EConst(s.toDouble) }

  def parens:Parser[Expr] = "(" ~> expr <~ ")"

  def term = ( value |  parens )

  // has to be btw value and sum so that they have precedence over sum
  def product = term * (
    "*" ^^^ { (a:Expr, b:Expr) => EMul(a,b) } |
      "/" ^^^ { (a:Expr, b:Expr) => EDiv(a,b) } )

  /*def sum = value ~ "+" ~ value ^^ { case left ~ "+" ~ right =>
    EAdd(left, right) } */

  // def sum = repsep(value,"+") ^^ { a:List[Expr] => ESum(a)

  /*def sum = value * (
    "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
    "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) }
  )*/

  def sum = product * (
    "+" ^^^ { (a:Expr, b:Expr) => EAdd(a,b) } |
      "-" ^^^ { (a:Expr, b:Expr) => ESub(a,b) }
    )

  def expr = ( sum | term )          //top level expression

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }

  def apply(s:String):Expr = {
    parse(s) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Bad syntax: "+s)
    }
  }

  //Simplify testing
  def test(exprstr: String) = {
    parse(exprstr) match {
      case Success(tree, _) =>
        println("Tree: "+tree)
        val v = tree.eval()
        println("Eval: "+v)
      case e: NoSuccess => Console.err.println(e)
    }
  }

  //A main method for testing
  def main(args: Array[String]) = test("4+5*2")
}
