package com.craftinginterpreters.scala.lox


class LoxClass(
  val name: String,
  val superclass: LoxClass,
  val methods: Map[String, LoxFunction])
  extends LoxCallable:
  //< Inheritance lox-class-constructor
  //< lox-class-methods
  //> lox-class-find-method
  def findMethod(name: String): LoxFunction =
    if (methods.contains(name)) return methods(name)
    //> Inheritance find-method-recurse-superclass
    if (superclass != null) return superclass.findMethod(name)
    //< Inheritance find-method-recurse-superclass
    null

  //< lox-class-find-method
  override def toString: String = name

  //> lox-class-call-arity
  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val instance = new LoxInstance(this)
    //> lox-class-call-initializer
    val initializer = findMethod("init")
    if (initializer != null) initializer.bind(instance).call(interpreter, arguments)
    //< lox-class-call-initializer
    instance

  override def arity: Int =
    /* Classes lox-class-call-arity < Classes lox-initializer-arity
        return 0;
    */
    //> lox-initializer-arity
    val initializer = findMethod("init")
    if (initializer == null) return 0
    initializer.arity
//< lox-initializer-arity
//< lox-class-call-arity

