package com.craftinginterpreters
package scala

object TestQuotes {

  import _root_.scala.quoted.*

  def oddEven(n: Expr[Int])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val number = n.valueOrAbort
//    println(n.asTerm.show)
    println(n.asTerm.show(using Printer.TreeStructure))
    number % 2 match {
      case 0 => Expr("even")
      case _ => Expr("odd")
    }
  }

  def oddEvenQuotes(n: Expr[Int])(using Quotes): Expr[String] = '{
    $n % 2 match {
      case 0 => "even"
      case _ => "odd"
    }
  }

  inline def oddEvenMacroQuote(inline number: Int): String =
    ${ oddEvenQuotes('number) }

  def getType[T](obj: Expr[T])(using t: Type[T])(using Quotes): Expr[String] = '{
    val o: t.Underlying = $obj
    o.getClass.getSimpleName
  }

  inline def getTypeMacro[T](obj: T): String = ${ getType('obj) }
}
