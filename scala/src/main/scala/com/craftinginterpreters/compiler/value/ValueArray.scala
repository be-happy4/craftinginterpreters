package com.craftinginterpreters.compiler.value

import scala.collection.mutable.ListBuffer


type ValueArray = ListBuffer[Value]

object ValueArray:
  def apply(): ValueArray = ListBuffer()

extension(x: ValueArray)
  def initValueArray = ???
  def writeValueArray = ???
  def freeValueArray = ???

