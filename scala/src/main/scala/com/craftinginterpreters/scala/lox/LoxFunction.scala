package com.craftinginterpreters.scala.lox


class LoxFunction(
  private val declaration: Stmt.Function, //> closure-field
  private val closure: Environment, //< closure-field
  /* Functions lox-function < Functions closure-constructor
    LoxFunction(Stmt.Function declaration) {
  */
  /* Functions closure-constructor < Classes is-initializer-field
    LoxFunction(Stmt.Function declaration, Environment closure) {
  */
  //> Classes is-initializer-field
  private val isInitializer: Boolean) extends LoxCallable:
  //< Classes is-initializer-field
  //> closure-constructor
  //< closure-constructor
  //> Classes bind-instance
  def bind(instance: LoxInstance): LoxFunction =
    val environment = new Environment(closure)
    environment.define("this", instance)
    /* Classes bind-instance < Classes lox-function-bind-with-initializer
        return new LoxFunction(declaration, environment);
    */
    //> lox-function-bind-with-initializer
    new LoxFunction(declaration, environment, isInitializer)
  //< lox-function-bind-with-initializer

  //< Classes bind-instance
  //> function-to-string
  override def toString: String = "<fn " + declaration.name.lexeme + ">"

  //< function-to-string
  //> function-arity
  override def arity: Int = declaration.function.params.size

  //< function-arity
  //> function-call
  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    /* Functions function-call < Functions call-closure
        Environment environment = new Environment(interpreter.globals);
    */
    //> call-closure
    val environment = new Environment(closure)
    //< call-closure
    for (i <- declaration.function.params.indices) {
      environment.define(declaration.function.params(i).lexeme, arguments(i))
    }
    /* Functions function-call < Functions catch-return
        interpreter.executeBlock(declaration.body, environment);
    */
    //> catch-return
    try interpreter.execute(declaration.function.body, environment)
    catch {
      case returnValue: Return =>

        //> Classes early-return-this
        if (isInitializer) return closure.getAt(0, "this")
        //< Classes early-return-this
        return returnValue.value
    }
    //< catch-return
    //> Classes return-this
    if (isInitializer) return closure.getAt(0, "this")
    //< Classes return-this
    null
//< function-call

