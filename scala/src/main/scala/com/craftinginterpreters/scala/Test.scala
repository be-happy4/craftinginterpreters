package com.craftinginterpreters.scala

import com.craftinginterpreters.scala.TestQuotes.oddEven

object Test {
  inline def v(x: Int) = ${ oddEven('x) }

  def main(args: Array[String]): Unit = {
    val args = 1;
    println(args)
    val a = "outer"
    {
      val a = "inner"
      println(a) // expect: inner
    }
    println(a) // expect: outer
  }
}
