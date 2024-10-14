package com.craftinginterpreters.scala.lox


case class LoxClass(
  name: String,
  superclass: Option[LoxClass],
  methods: Map[String, LoxFunction])
  extends LoxCallable:
  
  def findMethod(name: String): Option[LoxFunction] =
    methods.get(name) match
      case sm: Some[_] => sm
      case None => superclass.flatMap(_.findMethod(name))

  override def toString: String = name

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val instance = new LoxInstance(this)
    findMethod(TokenType.THIS.key) match
      case Some(initializer) => initializer.bind(instance).call(interpreter, arguments)
      case None =>
    instance

  override def arity: Int =
    findMethod(TokenType.THIS.key) match
      case Some(initializer) => initializer.arity
      case None => 0

