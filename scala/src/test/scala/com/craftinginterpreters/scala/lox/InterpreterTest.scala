package com.craftinginterpreters.scala.lox

import munit.Assertions.assertEquals

import java.io.{OutputStream, PrintStream}
import scala.Console.withOut
import scala.util.chaining.scalaUtilChainingOps

class InterpreterTest extends ParserTest:
  val interpreter = Interpreter()

  def interpret(first: String, more: String*): Unit =
    statements(first, more *)
      .pipe(interpreter.interpret)

  test("test logical operator for interpreter"):
    interpret("block", "scope")