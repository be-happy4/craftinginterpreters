package com.craftinginterpreters.scala.lox

import scala.collection.mutable


class Environment: //> environment-constructors
  //> enclosing-field
  final var enclosing: Environment = null
  //< enclosing-field
  final private val values = new mutable.HashMap[String, Any]()

  def this(enclosing: Environment) =
    this()
    this.enclosing = enclosing

  //< environment-constructors
  //> environment-get
  def get(name: Token): Option[Any] = {
    if (values.contains(name.lexeme)) return values.get(name.lexeme)
    //> environment-get-enclosing
    if (enclosing != null) return enclosing.get(name)
    //< environment-get-enclosing
    throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
  }

  //< environment-get
  //> environment-assign
  def assign(name: Token, value: Any): Unit = {
    if (values.contains(name.lexeme)) {
      values.put(name.lexeme, value)
      return
    }
    //> environment-assign-enclosing
    if (enclosing != null) {
      enclosing.assign(name, value)
      return
    }
    //< environment-assign-enclosing
    throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
  }

  //< environment-assign
  //> environment-define
  def define(name: String, value: Any): Unit =
    values(name) = value

  //< environment-define
  //> Resolving and Binding ancestor
  def ancestor(distance: Int): Environment =
    var environment = this
    for (_ <- 0 until distance) {
      environment = environment.enclosing // [coupled]
    }
    environment

  //< Resolving and Binding ancestor
  //> Resolving and Binding get-at
  def getAt(distance: Int, name: String): Option[Any] = ancestor(distance).values.get(name)

  //< Resolving and Binding get-at
  //> Resolving and Binding assign-at
  def assignAt(distance: Int, name: Token, value: Any): Unit =
    ancestor(distance).values.put(name.lexeme, value)

  //< Resolving and Binding assign-at
  //> omit
  override def toString: String =
    var result = values.toString
    if (enclosing != null) result += " -> " + enclosing.toString
    result
//< omit
