package com.craftinginterpreters.scala

object Test:

  def main(args: Array[String]): Unit =
    val a = Some(1).collect {
      case 1 => null
    }
    println(a)


  class A private():
    def this(a: Int) =
      this()
      println(super.clone())
  object A

