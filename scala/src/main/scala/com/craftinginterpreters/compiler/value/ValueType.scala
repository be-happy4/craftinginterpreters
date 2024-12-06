package com.craftinginterpreters.compiler.value

//enum ValueType[T]:
//  case VAL_BOOL extends ValueType[Boolean]
//  case VAL_NIL extends ValueType[Unit]
//  case VAL_NUMBER extends ValueType[Double]
//  case VAL_OBJ extends ValueType[Any]

sealed trait ValueType:
  type T
  def isBool: Boolean = this match
    case ValueType.VAL_BOOL => true
    case _ => false
  def isNil: Boolean = this match
    case ValueType.VAL_NIL => true
    case _ => false
  def isNumber: Boolean = this match
    case ValueType.VAL_NUMBER => true
    case _ => false
  def isObj: Boolean = this match
    case ValueType.VAL_OBJ => true
    case _ => false

object ValueType:
  case object VAL_BOOL extends ValueType:
    type T = Boolean

  case object VAL_NIL extends ValueType:
    type T = Null

  case object VAL_NUMBER extends ValueType:
    type T = Double

  case object VAL_OBJ extends ValueType:
    type T = AnyRef

