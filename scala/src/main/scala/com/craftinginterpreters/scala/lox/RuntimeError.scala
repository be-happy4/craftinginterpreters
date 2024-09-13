package com.craftinginterpreters.scala.lox

class RuntimeError(val token: Token, message: String) extends RuntimeException(message) {
}
