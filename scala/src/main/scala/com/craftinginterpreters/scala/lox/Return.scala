package com.craftinginterpreters.scala.lox

//> Functions return-exception
class Return private[lox](private[lox] val value: AnyRef) extends RuntimeException(null, null, false, false) {
}
