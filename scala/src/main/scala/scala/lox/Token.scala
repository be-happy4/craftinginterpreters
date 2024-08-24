package com.craftinginterpreters
package scala.lox

class Token private[lox](
                          private[lox] val tt: TokenType,
                          private[lox] val lexeme: String,
                          private[lox] val literal: Any,
                          private[lox] val line: Int // [location]
                        ):
  override def toString: String = f"$tt $lexeme $literal"

