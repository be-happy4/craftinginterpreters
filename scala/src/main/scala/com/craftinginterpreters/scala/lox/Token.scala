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
  val literal: Any = Token.dummy,
  val line: Int = -1 // [location]
):
  def this(typ: TokenType) =
    this(typ, typ.key, null)
  override def toString: String = f"$typ $lexeme $literal"

object Token:
  final val dummy = new Object()
  final val THIS = new Token(TokenType.THIS, TokenType.THIS.key)
  final val SUPER = new Token(TokenType.SUPER, TokenType.SUPER.key)

