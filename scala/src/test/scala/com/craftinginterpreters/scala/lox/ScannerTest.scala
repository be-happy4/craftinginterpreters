package com.craftinginterpreters.scala.lox

import com.craftinginterpreters.scala.BaseTest
import com.craftinginterpreters.scala.lox.TokenTest.{assertTokens, parseTokens}

import scala.util.chaining.scalaUtilChainingOps

class ScannerTest extends BaseTest:
  def tokens(first: String, more: String*): List[Token] =
    source(first, more *)
      .pipe(scan)

  test("test block comments"):
    assertTokens(
      tokens("comments", "block_comment"),
      parseTokens("print,string 1,print,string 2, print,string 3")
    )
    assertTokens(
      tokens("comments", "block_comment_unicode"),
      parseTokens("print,string ok,semicolon")
    )

  test("test logical operator"):
    assertTokens(
      tokens("logical_operator", "mixed"),
      parseTokens("""
        |number 1,and,number 2,or,number 3,and,number 4,semicolon,
        |number 5,or,number 6,and,number 7,or,number 8,semicolon,
        |number 1,or,number 2,or,number 3,and,number 4,or,number 5,or,number 6,semicolon
        |""".stripMargin.replaceAll("\n", ""))
    )

  test("test comma expression"):
    assertTokens(
      tokens("comma", "comma"),
      parseTokens("""
        |number 1,comma,number 2,comma,number 3,semicolon,
        |number 1,EQUAL_EQUAL,number 2,comma,number 3,GREATER,number 4,or,number 5,LESS,number 6,semicolon
        |""".stripMargin.replaceAll("\n", ""))
    )

  test("test ternary expression"):
    assertTokens(
      tokens("ternary", "ternary"),
      parseTokens("""
        |number 1,EQUAL_EQUAL,number 2,QUESTION,NUMBER 3,COLON,NUMBER 4,semicolon,
        |number 1,LESS,number 2,QUESTION,number 3,GREATER,number 4,QUESTION,number 5,
        |COLON,number 6,COLON,number 7,semicolon
        |""".stripMargin.replaceAll("\n", ""))
    )


