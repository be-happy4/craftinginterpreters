package com.craftinginterpreters.scala.lox

import scala.collection.mutable


class LoxInstance(val klass: LoxClass):

  final private val fields = new mutable.HashMap[String, Any]

  def apply(name: Token): Any =
    fields.get(name.lexeme) match
      case Some(m) => m
      case None => klass.findMethod(name.lexeme) match
        case Some(method) => method.bind(this)
        case None => throw new RuntimeError(name, 
          "Undefined property '" + name.lexeme + "'.")

  def update(name: Token, value: Any): Unit =
    fields(name.lexeme) = value

  override def toString: String = klass.name + " instance"

