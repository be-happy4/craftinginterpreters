package com.craftinginterpreters
package scala.lox

/**
 * lox token
 * @see [[com.sun.tools.javac.parser.Tokens]]
 * @see [[com.sun.tools.javac.parser.Tokens.TokenKind]]
 */
class Token private[lox](
  private[lox] val typ: TokenType,
  private[lox] val lexeme: String,
  private[lox] val literal: Any,
  private[lox] val line: Int // [location]
):
  override def toString: String = f"$typ $lexeme $literal"

