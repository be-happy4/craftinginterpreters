package com.craftinginterpreters.scala.lox

import com.craftinginterpreters.scala.lox.ParserTest.assertASTs
import com.craftinginterpreters.scala.lox.TokenType.{EQUAL_EQUAL, GREATER, LESS}
import munit.Assertions.assertEquals

import scala.util.chaining.scalaUtilChainingOps

object ParserTest:
  val printer = AstPrinter()

  def assertAST(obtained: Stmt, expected: Stmt): Unit =
    assertEquals(
      printer.print(obtained).replace(".0", ""),
      printer.print(expected))

  def assertASTs(obtained: List[Stmt], expected: List[Stmt]): Unit =
    assertEquals(obtained.size, expected.size)
    (obtained zip expected).foreach(assertAST)


class ParserTest extends ScannerTest:
  def statements(first: String, more: String*): List[Stmt] =
    tokens(first, more *)
      .pipe(parse)

  test("test logical operator for parser"):
    val expected = List(
      Expr.Logical(
        Expr.Logical(Expr.Literal(1), Token(TokenType.AND), Expr.Literal(2)),
        Token(TokenType.OR),
        Expr.Logical(Expr.Literal(3), Token(TokenType.AND), Expr.Literal(4)),
      ),
      Expr.Logical(
        Expr.Logical(
          Expr.Literal(5),
          Token(TokenType.OR),
          Expr.Logical(
            Expr.Literal(6),
            Token(TokenType.AND),
            Expr.Literal(7))),
        Token(TokenType.OR),
        Expr.Literal(8)),
      Expr.Logical(
        Expr.Logical(
          Expr.Logical(
            Expr.Logical(
              Expr.Literal(1),
              Token(TokenType.OR),
              Expr.Literal(2)),
            Token(TokenType.OR),
            Expr.Logical(
              Expr.Literal(3),
              Token(TokenType.AND),
              Expr.Literal(4))),
          Token(TokenType.OR),
          Expr.Literal(5)),
        Token(TokenType.OR),
        Expr.Literal(6)),
    )
    expected.map(ParserTest.printer.print).foreach(println)
    assertASTs(statements("logical_operator", "mixed"), expected)

  test("test ternary expression for parser"):
    val expected = List(
      Expr.Ternary(
        Expr.Logical(
          Expr.Literal(1),
          Token(EQUAL_EQUAL),
          Expr.Literal(2)
        ),
        Expr.Literal(3),
        Expr.Literal(4)
      ),
      Expr.Ternary(
        Expr.Logical(
          Expr.Literal(1),
          Token(LESS),
          Expr.Literal(2)
        ),
        Expr.Ternary(
          Expr.Logical(
            Expr.Literal(3),
            Token(GREATER),
            Expr.Literal(4)
          ),
          Expr.Literal(5),
          Expr.Literal(6),
        ),
        Expr.Literal(7)))
    expected.map(ParserTest.printer.print).foreach(println)
    assertASTs(statements("ternary", "ternary"), expected)
