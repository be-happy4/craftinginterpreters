package com.craftinginterpreters
package scala.lox

class RuntimeError private[lox](private[lox] val token: Token, message: String) extends RuntimeException(message) {
}
