package com.craftinginterpreters.scala.lox


class LoxClass(
  val name: String,
  val superclass: LoxClass,
  val methods: Map[String, LoxFunction])
  extends LoxCallable:
  
  def findMethod(name: String): LoxFunction =
    if (methods.contains(name)) methods(name)
    else if (superclass != null) superclass.findMethod(name)
    else null

  override def toString: String = name

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val instance = new LoxInstance(this)
    val initializer = findMethod(TokenType.THIS.key)
    if (initializer != null) initializer.bind(instance).call(interpreter, arguments)
    instance

  override def arity: Int =
    /* Classes lox-class-call-arity < Classes lox-initializer-arity
        return 0;
    */
    val initializer = findMethod(TokenType.THIS.key)
    if (initializer == null) 0
    else initializer.arity

