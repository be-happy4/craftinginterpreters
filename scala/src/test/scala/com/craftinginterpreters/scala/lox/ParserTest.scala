package com.craftinginterpreters.scala.lox

import com.craftinginterpreters.scala.lox.ParserTest.assertASTs
import munit.Assertions.assertEquals

import scala.util.chaining.scalaUtilChainingOps

object ParserTest:
  val printer = AstPrinter()

  def assertAST(obtained: Stmt | Expr, expected: Stmt | Expr): Unit =
    def printStmtOrExpr(value: Stmt | Expr): String =
      value match
        case x: Stmt => printer.print(x)
        case x: Expr => printer.print(x)

    assertEquals(printStmtOrExpr(obtained).replace(".0", ""), printStmtOrExpr(expected))

  def assertASTs(obtained: List[Stmt | Expr], expected: List[Stmt | Expr]): Unit =
    assertEquals(obtained.size, expected.size)
    (obtained zip expected).foreach(assertAST)


class ParserTest extends ScannerTest:
  def statements(first: String, more: String*): List[Stmt] =
    tokens(first, more *)
      .pipe(parse)

  test("test logical operator for parser"):
    val expected = List(
      Stmt.Expression(
        Expr.Logical(
          Expr.Logical(Expr.Literal(1), Token(TokenType.AND), Expr.Literal(2)),
          Token(TokenType.OR),
          Expr.Logical(Expr.Literal(3), Token(TokenType.AND), Expr.Literal(4)),
        )),
      Stmt.Expression(
        Expr.Logical(
          Expr.Logical(
            Expr.Literal(5),
            Token(TokenType.OR),
            Expr.Logical(
              Expr.Literal(6),
              Token(TokenType.AND),
              Expr.Literal(7))),
          Token(TokenType.OR),
          Expr.Literal(8)))
    )
    expected.map(ParserTest.printer.print).foreach(println)
    assertASTs(statements("logical_operator", "mixed"), expected)
