package com.craftinginterpreters.scala.lox

import scala.util.chaining.scalaUtilChainingOps

class LoxTest extends InterpreterTest:
  val resolver = Resolver(interpreter)

  def run(first: String, more: String*): Unit =
    val stmt = statements(first, more *)
    stmt.pipe(resolver.resolve)
    interpreter.interpret(stmt)

  def fromOut(thunk: => Unit): String =
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      thunk
      output.toString
    }

  def fromErr(thunk: => Unit): String =
    val output = new java.io.ByteArrayOutputStream()
    Console.withErr(output) {
      thunk
      output.toString
    }

  test("test lambda"):
    assertEquals(
      fromOut(run("lambda", "lambda_param")),
      "2\n3\n")

  test("test function"):
    assertEquals(
      fromOut(run("function", "parameters")),
      "0\n1\n3\n6\n10\n15\n21\n28\n36\n")

  test("test closure"):
    assertEquals(fromOut(run("closure", "assign_to_shadowed_later")),
      "inner\nassigned\n")

  test("test class"):
    assertEquals(fromOut(run("class", "person")),
      "Jane\n")

  test("test super"):
    assertEquals(fromOut(run("super", "super_in_inherited_method")),
      "A\n")
    assertEquals(fromOut(run("super", "indirectly_inherited")),
      "C.foo()\nA.foo()\n")


