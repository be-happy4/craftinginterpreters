package com.craftinginterpreters.scala

object Test:
  def f =
    if (1 == 1) 2 else ()
  def f1(): Unit =
    if (1 == 1) 2 else ()
  def f2(): Any =
    if (1 == 1) 2 else ()
  def g =
    if (1 == 1) 2
  def g1(): Unit =
    if (1 == 1) 2
  def g2(): Any =
    if (1 == 1) 2

  def main(args: Array[String]): Unit =
    var x = if (1 == 1) 2 else ()
    var x1: Unit = if (1 == 1) 2 else ()
    var x2: Any = if (1 == 1) 2 else ()

    var y = if (1 == 1) 2
    var y1: Unit = if (1 == 1) 2
    var y2: Any = if (1 == 1) 2

    println(x1)
    println(y1)
    println(x2)
    println(y2)


  def unit: Any = {
    for i <- 1 until 10 do
      i
  }

