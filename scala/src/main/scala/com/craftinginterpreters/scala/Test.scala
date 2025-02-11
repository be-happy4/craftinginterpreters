package com.craftinginterpreters.scala

import java.time.Duration

object Test:
//  def fact(0: Int): Int = 1
  def fact(n: Int): Int = n match
    case 0 => 1
    case _ => n * fact(n - 1)
  def main(args: Array[String]): Unit =
    val halfMarathonDistance = 21.0975
    val expectedTime = Duration.ofHours(2).plusMinutes(45)




  class A private():
    def this(a: Int) =
      this()
      println(super.clone())
  object A

extension[T](l: List[T])
  def take2(n: Int): List[T] = (n, l) match
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (_, x :: xs) => x :: xs.take(n - 1)


def buildString(builder: => StringBuilder = new StringBuilder())
  (f: StringBuilder ?=> Unit): String =
  val sb = builder
  given StringBuilder = sb
  f
  sb.toString

extension (s: Any)
  infix def unary_+(using builder: StringBuilder): Unit =
    builder.append(s)
