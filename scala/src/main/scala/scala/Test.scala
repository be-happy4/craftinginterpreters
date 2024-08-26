package com.craftinginterpreters
package scala

import scala.TestQuotes.{oddEven, oddEvenQuotes}

object Test {
  inline def v(x: Int) = ${ oddEven('x) }
  def main(args: Array[String]): Unit = {
    println(TestQuotes.oddEvenMacroQuote(10))
    println(v(9))
    println(valueOf[23])
    
  }
}
