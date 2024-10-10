package com.craftinginterpreters.scala.lox


class LoxFunction(
  val name: String,
  private val declaration: Expr.Function, //> closure-field
  private val closure: Env, //< closure-field
  /* Functions lox-function < Functions closure-constructor
    LoxFunction(Stmt.Function declaration) {
  */
  /* Functions closure-constructor < Classes is-initializer-field
    LoxFunction(Stmt.Function declaration, Environment closure) {
  */
  private val isInitializer: Boolean) extends LoxCallable:

  def bind(instance: LoxInstance): LoxFunction =
    val environment = new Env(closure)
    environment.define("this", instance)
    /* Classes bind-instance < Classes lox-function-bind-with-initializer
        return new LoxFunction(declaration, environment);
    */
    new LoxFunction(name, declaration, environment, isInitializer)

  override def toString: String = "<fn " + name + ">"

  override def arity: Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    /* Functions function-call < Functions call-closure
        Environment environment = new Environment(interpreter.globals);
    */
    val env = new Env(closure)
    for (i <- declaration.params.indices) {
      env.define(arguments(i))
    }
    /* Functions function-call < Functions catch-return
        interpreter.executeBlock(declaration.body, environment);
    */
    var res: Any = ()
    try
      res = interpreter.executeBlock(declaration.body match
        case x: Stmt.Block => x.statements
        case v => List(v), env)
    catch
      case returnValue: Return => res = returnValue.value
    if (isInitializer)
      // Classes return-this
      res = closure.getAt(0, Env.SLOT_THIS)
    res

