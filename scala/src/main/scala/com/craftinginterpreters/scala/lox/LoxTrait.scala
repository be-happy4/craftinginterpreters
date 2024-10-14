package com.craftinginterpreters.scala.lox

case class LoxTrait(
  name: Token,
  methods: Map[String, LoxFunction]):

  override def toString: String =
    name.lexeme;

