package com.craftinginterpreters.scala.lox

import scala.collection.mutable


class LoxInstance(val klass: LoxClass): //< lox-instance-fields

  //> lox-instance-fields
  final private val fields = new mutable.HashMap[String, Any]

  //> lox-instance-get-property
  def get(name: Token): Any =
    if (fields.contains(name.lexeme)) return fields(name.lexeme)
    //> lox-instance-get-method
    val method = klass.findMethod(name.lexeme)
    /* Classes lox-instance-get-method < Classes lox-instance-bind-method
        if (method != null) return method;
    */
    //> lox-instance-bind-method
    if (method != null) return method.bind(this)
    //< lox-instance-bind-method
    //< lox-instance-get-method
    throw new RuntimeError(name, // [hidden]
      "Undefined property '" + name.lexeme + "'.")

  //< lox-instance-get-property
  //> lox-instance-set-property
  def set(name: Token, value: Any): Unit =
    fields.put(name.lexeme, value)

  //< lox-instance-set-property
  override def toString: String = klass.name + " instance"

