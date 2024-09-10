package com.craftinginterpreters.scala.lox

import com.craftinginterpreters.scala.BaseTest
import com.craftinginterpreters.scala.lox.TokenTest.{DEFAULT_OPTS, assertToken, parseToken}
import munit.Assertions.assertEquals
import munit.Compare

import scala.annotation.tailrec

object TokenTest:
  case class AssertTokenOptions(
    ignoreLiteral: Boolean = false,
    ignoreLexeme: Boolean = true,
    ignoreLine: Boolean = true
  )

  implicit val DEFAULT_OPTS: AssertTokenOptions = AssertTokenOptions()

  def parseToken(str: String): Token =
    val parts = str.trim.split(' ')
    val typ = TokenType.valueOf(parts(0).toUpperCase)
    val literal = if parts.length > 1 then parts(1) else null
    val line = if parts.length > 2 then Integer.parseInt(parts(2)) else -1
    Token(typ, null, literal, line)

  def parseTokens(str: String, separator: String = ","): List[Token] =
    str.split(separator).map(parseToken).toList


  def assertToken(obtained: Token, expected: Token)(implicit opts: AssertTokenOptions = DEFAULT_OPTS): Unit =
    assertEquals(obtained.typ, expected.typ)
    if !opts.ignoreLiteral then
      obtained.typ match
        // also check literal
        case TokenType.STRING =>
          assertEquals(obtained.literal.toString, expected.literal.toString)
        case TokenType.NUMBER =>
          assertEquals(BigDecimal(0), BigDecimal(obtained.literal.toString) - BigDecimal(expected.literal.toString))
        case _ =>
    if !opts.ignoreLexeme then
      assertEquals(obtained.lexeme, expected.lexeme)
    if !opts.ignoreLine then
      assertEquals(obtained.line, expected.line)

  def assertTokens(obtained: Seq[Token], expected: Seq[Token])(implicit opts: AssertTokenOptions = DEFAULT_OPTS): Unit =
    val obtained0 = trimEOF(obtained)
    val expected0 = trimEOF(expected)
    assertEquals(obtained0.size, expected0.size)
    (obtained0 zip expected0).foreach(assertToken)

  @tailrec
  def trimEOF(tokens: Seq[Token]): Seq[Token] =
    if tokens.last.typ == TokenType.EOF then trimEOF(tokens.dropRight(1))
    else tokens

class TokenTest extends BaseTest:
  test("test token equals"):
    assertToken(Token(TokenType.IF, null, "if"), parseToken("IF if"))
    assertToken(Token(TokenType.STRING, "\"2\"", "2"), parseToken("STRING \"2\" 2"))


