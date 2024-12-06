package com.craftinginterpreters.compiler.value

class Value(typ: ValueType)(as: typ.T):
  override def toString: String = super.toString
