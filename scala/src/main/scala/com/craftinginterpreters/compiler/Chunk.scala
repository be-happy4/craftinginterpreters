package com.craftinginterpreters.compiler

import com.craftinginterpreters.compiler.value.{Value, ValueArray}

import java.lang.classfile.Opcode
import scala.collection.mutable.ListBuffer

/**
 * @see [[Opcode]]
 */
class Chunk(
  protected val code: ListBuffer[Opcode] = ListBuffer(),
  protected val lines: ListBuffer[Int] = ListBuffer(),
  protected val constants: ValueArray = ValueArray()):
  def this() =
    this(ListBuffer(), ListBuffer(), ListBuffer())

  def writeChunk(byte: Opcode): Unit =
    this.code += byte

  def addConstant(value: Value): Int =
    constants += value
    constants.size - 1
