package com.craftinginterpreters.scala

import TestQuotes.{oddEven, oddEvenQuotes}

import java.nio.file.Path
import java.nio.file.Files
import scala.util.chaining.scalaUtilChainingOps

object Test {
  inline def v(x: Int) = ${ oddEven('x) }

  def main(args: Array[String]): Unit = {
    println(
      "path".pipe(_ + ".lox").pipe(Path.of(_)).pipe(Files.exists(_)))

  }
}
