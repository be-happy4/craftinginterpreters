package com.craftinginterpreters.scala.lox

import scala.collection.mutable

object Env:
  val SLOT_THIS = 0
  val SLOT_SUPER = 0

class Env(val enclosing: Env = null):
  private val values = new mutable.ListBuffer[Any]()

//  def get(name: Token): Option[Any] = {
//    if (values.contains(name.lexeme)) return values.get(name.lexeme)
//    if (enclosing != null) return enclosing.get(name)
//    throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
//  }
////
//  def assign(name: Token, slot: Int, value: Any): Unit = {
//    if (values.contains(name.lexeme)) {
//      values.put(name.lexeme, value)
//      return
//    }
//    if (enclosing != null) {
//      enclosing.assign(name, slot, value)
//      return
//    }
//    throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
//  }

  def define(value: Any): Int =
    values += value
    values.size - 1

  private def ancestor(distance: Int): Env =
    var environment = this
    for (_ <- 0 until distance) {
      environment = environment.enclosing // [coupled]
    }
    environment

  def getAt(distance: Int, slot: Int): Any =
    ancestor(distance).values(slot)

  def assignAt(distance: Int, slot: Int, value: Any): Unit =
    ancestor(distance).values(slot) = value

  override def toString: String =
    var result = values.toString
    if (enclosing != null) result += " -> " + enclosing.toString
    result
