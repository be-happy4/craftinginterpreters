package com.craftinginterpreters.scala.lox

/**
 * lox token
 *
 * @see [[com.sun.tools.javac.parser.Tokens]]
 * @see [[com.sun.tools.javac.parser.Tokens.TokenKind]]
 */
class Token(
  val typ: TokenType,
  val lexeme: String,
  val literal: Any = Token.UNINITIATED,
  val line: Int = -1 // [location]
):
  def this(typ: TokenType) =
    this(typ, typ.key, null)
  override def toString: String = f"$typ $lexeme $literal"

object Token:
  final val UNINITIATED = new Object()
  final val DUMMY = new Token(TokenType.EOF, "")

