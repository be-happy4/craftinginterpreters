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
      parseTokens("number 1,and,number 2,or,number 3,and,number 4,semicolon,"
        + "number 5,or,number 6,and,number 7,or,number 8,semicolon")
    )


