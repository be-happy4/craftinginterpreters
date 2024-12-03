package com.craftinginterpreters.scala

import com.craftinginterpreters.scala.lox.Lox

import java.time.{Duration, LocalDateTime}

object Test:

  def main(args: Array[String]): Unit =
    println(LocalDateTime.now())
    val startTime = System.nanoTime()
    Lox.run("""
      |fun fib(n) {
      |  if (n < 2) return n;
      |  return fib(n - 1) + fib(n - 2);
      |}
      |
      |var before = clock();
      |print fib(40);
      |var after = clock();
      |print after - before;""".stripMargin)
    val endTime = System.nanoTime()
    println(Duration.ofNanos(endTime - startTime))


  class A private():
    def this(a: Int) =
      this()
      println(super.clone())
  object A

def buildString(builder: => StringBuilder = new StringBuilder())
  (f: StringBuilder ?=> Unit): String =
  val sb = builder
  given StringBuilder = sb
  f
  sb.toString

extension (s: Any)
  infix def unary_+(using builder: StringBuilder): Unit =
    builder.append(s)
