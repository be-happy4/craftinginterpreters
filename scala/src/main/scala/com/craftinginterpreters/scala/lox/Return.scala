package com.craftinginterpreters.scala.lox

//> Functions return-exception
class Return(val value: Any) extends RuntimeException(null, null, false, false)
