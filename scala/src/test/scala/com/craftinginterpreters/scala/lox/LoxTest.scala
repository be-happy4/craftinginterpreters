package com.craftinginterpreters.scala.lox

import scala.util.chaining.scalaUtilChainingOps

class LoxTest extends InterpreterTest:
  val resolver = Resolver(interpreter)
  val output = new java.io.ByteArrayOutputStream()

  def run(first: String, more: String*): Unit =
    val stmt = statements(first, more *)
    stmt.pipe(resolver.resolve)
    interpreter.interpret(stmt)

  def fromOut(thunk: => Unit): String =
    Console.withOut(output) {
      thunk
      output.toString
    }

  def fromErr(thunk: => Unit): String =
    Console.withErr(output) {
      thunk
      output.toString
    }

  test("test for lambda"):
    assertEquals(fromOut(run("lambda", "lambda_param")), "2\n3\n")

  test("test for closure"):
    assertEquals(fromOut(run("closure", "assign_to_shadowed_later")),
      "inner\nassigned\n")


