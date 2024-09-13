package com.craftinginterpreters.scala.lox


trait LoxCallable:
  //> callable-arity
  def arity: Int

  //< callable-arity
  def call(interpreter: Interpreter, arguments: List[Any]): Any
