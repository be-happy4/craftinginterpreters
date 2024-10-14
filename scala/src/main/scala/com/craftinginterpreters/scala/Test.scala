package com.craftinginterpreters.scala

object Test:

  def main(args: Array[String]): Unit =
    val x = 1 -> 2 -> 3
    println(x._1)


  class A private():
    def this(a: Int) =
      this()
      println(super.clone())
  object A

