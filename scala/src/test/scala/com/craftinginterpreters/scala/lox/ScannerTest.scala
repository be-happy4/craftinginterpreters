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


