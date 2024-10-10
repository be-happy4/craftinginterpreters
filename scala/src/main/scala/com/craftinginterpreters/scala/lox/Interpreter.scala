package com.craftinginterpreters.scala.lox



import com.craftinginterpreters.scala.lox.Interpreter.stringify
import com.craftinginterpreters.scala.lox.TokenType.*

import java.util.Objects
import scala.collection.mutable


/* Evaluating Expressions interpreter-class < Statements and State interpreter
class Interpreter implements Expr.Visitor<Object> {
*/
class Interpreter extends Expr.Visitor[Any]:

  /* Statements and State env-field < Functions global-env
    private Environment env = new Environment();
  */
  private final val globals = new mutable.HashMap[String, Any]()
  private var env: Env = null
  private val locals = new mutable.HashMap[Expr, Int]()
  private val slots = new mutable.HashMap[Stmt, Int]()

  globals("clock") = new LoxCallable() {
    override def arity = 0

    override def call(interpreter: Interpreter, arguments: List[Any]): Double =
      System.nanoTime() / 1000000.0

    override def toString = "<native fn>"
  }

  /* Evaluating Expressions interpret < Statements and State interpret
    void interpret(Expr expression) { // [void]
      try {
        Object value = evaluate(expression);
        println(stringify(value));
      } catch (RuntimeError error) {
        Lox.runtimeError(error);
      }
    }
  */
  def interpret(statements: List[Stmt]): List[Any] =
    try {
      statements.map(evaluate)
    } catch {
      case error: RuntimeError =>
        Lox.runtimeError(error)
        List.empty
    }

  private def evaluate(stmt: Stmt): Any =
    stmt.accept(this)

  def resolve(expr: Expr, depth: Int, slot: Int): Unit =
    locals(expr) = depth
    slots(expr) = slot

  def executeBlock(stmts: List[Stmt], env: Env): Any =
    val previous = this.env
    try {
      this.env = env
      var res: Any = ()
      for (statement <- stmts) {
        res = evaluate(statement)
      }
      res
    } finally this.env = previous

  override def visitBlockStmt(stmt: Stmt.Block): Any =
    executeBlock(stmt.statements, new Env(env))

  override def visitClassStmt(stmt: Stmt.Class): Unit =
    var superclass: Any = null
    if (stmt.superclass != null) {
      superclass = evaluate(stmt.superclass)
      if (!superclass.isInstanceOf[LoxClass])
        throw new RuntimeError(stmt.superclass.name, "Superclass must be a class.")
    }
    define(stmt.name, null)
    if (stmt.superclass != null) {
      env = new Env(env)
      env.define(superclass)
    }
    val methods = new mutable.HashMap[String, LoxFunction]
    for (method <- stmt.methods) {
      /* Classes interpret-methods < Classes interpreter-method-initializer
            LoxFunction function = new LoxFunction(method, env);
      */
      val function = new LoxFunction(method.name.lexeme, method.function, env, method.name.lexeme.equals("init"))
      methods(method.name.lexeme) = function
    }
    /* Classes interpret-methods < Inheritance interpreter-construct-class
        LoxClass klass = new LoxClass(stmt.name.lexeme, methods);
    */
    val klass = new LoxClass(stmt.name.lexeme, superclass.asInstanceOf[LoxClass], methods.toMap)
    if (superclass != null) env = env.enclosing
    /* Classes interpreter-visit-class < Classes interpret-methods
        LoxClass klass = new LoxClass(stmt.name.lexeme);
    */
    // TODO: Class slot
    assign(stmt.name, 0, klass)

  override def visitFunctionStmt(stmt: Stmt.Function): Unit =
    val function = new LoxFunction(stmt.name.lexeme, stmt.function, env, false)
    define(stmt.name, function)

  override def visitIfStmt(stmt: Stmt.If): Any =
    if (isTruthy(evaluate(stmt.condition))) evaluate(stmt.thenBranch)
    else if (stmt.elseBranch != null) evaluate(stmt.elseBranch)
    else ()

  override def visitPrintStmt(stmt: Stmt.Print): Unit =
    val value = evaluate(stmt.expression)
    println(stringify(value))

  override def visitReturnStmt(stmt: Stmt.Return): Unit =
    throw new Return(Option(stmt.value).map(evaluate).orNull)

  override def visitVarStmt(stmt: Stmt.Var): Unit =
    var value: Any = Token.UNINITIATED
    if (stmt.initializer != null)
      value = evaluate(stmt.initializer)
    define(stmt.name, value)

  override def visitWhileStmt(stmt: Stmt.While): Unit = try {
    while (isTruthy(evaluate(stmt.condition))) evaluate(stmt.body)
  } catch
    case _: LoopBreakerException => // ignored

  override def visitAssignExpr(expr: Expr.Assign): Unit =
    val value = evaluate(expr.value)
    /* Statements and State visit-assign < Resolving and Binding resolved-assign
        env.assign(expr.name, value);
    */
    locals.get(expr) match
      case Some(distance) => env.assignAt(distance, slots(expr), value)
      case None if globals.contains(expr.name.lexeme) =>
        globals(expr.name.lexeme) = value
      case None =>
        throw new RuntimeError(expr.name, "Undefined variable '" + expr.name.lexeme + "'.")

  override def visitBinaryExpr(expr: Expr.Binary): Any =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right) // [left]

    expr.operator.typ match
      case BANG_EQUAL =>
        !Objects.equals(left, right)
      case EQUAL_EQUAL =>
        Objects.equals(left, right)
      case GREATER =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl > dr
      case GREATER_EQUAL =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl >= dr
      case LESS =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl < dr
      case LESS_EQUAL =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl <= dr
      case MINUS =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl - dr
      case PLUS =>
        (left, right) match
          case (dl: Double, dr: Double) => dl + dr
          case (dl: String, _) => dl + stringify(right)
          case (_, dr: String) => stringify(left) + dr
          case _ => throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      case SLASH =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl / dr
      case STAR =>
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        dl * dr
      case _ => throw MatchError(s"${expr.operator}")

  override def visitCallExpr(expr: Expr.Call): Any =
    val callee = evaluate(expr.callee)
    val arguments = new mutable.ListBuffer[Any]()
    for (argument <- expr.arguments) { // [in-order]
      arguments += evaluate(argument)
    }
    callee match
      case function: LoxCallable =>
        if (arguments.size ne function.arity) throw new RuntimeError(expr.paren,
          s"Expected ${function.arity} arguments but got ${arguments.size}.")
        function.call(this, arguments.toList)
      case _ => throw new RuntimeError(expr.paren, "Can only call functions and classes.")

  override def visitGetExpr(expr: Expr.Get): Any = evaluate(expr.obj) match
    case value: LoxInstance => value(expr.name)
    case _ => throw new RuntimeError(expr.name, "Only instances have properties.")

  override def visitGroupingExpr(expr: Expr.Grouping): Any = evaluate(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Any = expr.value

  override def visitLogicalExpr(expr: Expr.Logical): Any =
    val left = evaluate(expr.left)
    if (expr.operator.typ eq TokenType.OR) if (isTruthy(left)) return left
    else if (!isTruthy(left)) return left
    evaluate(expr.right)

  override def visitSetExpr(expr: Expr.Set): Any =
    val obj = evaluate(expr.obj)
    obj match
      case inst: LoxInstance =>
        val value = evaluate(expr.value)
        inst(expr.name) = value
        value
      case _ => throw new RuntimeError(expr.name, "Only instances have fields.")

  override def visitSuperExpr(expr: Expr.Super): Any =
    val distance = locals(expr)
    val superclass = env.getAt(distance, Env.SLOT_SUPER).asInstanceOf[LoxClass]
    val obj = env.getAt(distance - 1, Env.SLOT_THIS).asInstanceOf[LoxInstance]
    val method = superclass.findMethod(expr.method.lexeme)
    if (method == null) throw new RuntimeError(expr.method, "Undefined property '" + expr.method.lexeme + "'.")
    method.bind(obj)

  override def visitThisExpr(expr: Expr.This): Any =
    lookUpVariable(expr.keyword, expr)

  override def visitUnaryExpr(expr: Expr.Unary): Any =
    val right = evaluate(expr.right)
    expr.operator.typ match
      case BANG =>
        !isTruthy(right)
      case MINUS =>
        checkNumberOperand(expr.operator, right)
        -right.asInstanceOf[Double]
      // Unreachable.
      case _ => null

  override def visitVariableExpr(expr: Expr.Variable): Any =
    /* Statements and State visit-variable < Resolving and Binding call-look-up-variable
        return env.get(expr.name);
    */
    lookUpVariable(expr.name, expr) match
      case Token.UNINITIATED => throw new RuntimeError(expr.name,
          s"Variable '${expr.name.lexeme}' not initialized.")
      case v => v

  private def lookUpVariable(name: Token, expr: Expr): Any =
    locals.get(expr) match
      case Some(distance) => env.getAt(distance, slots(expr))
      case None => globals.getOrElse(name.lexeme, throw new RuntimeError(name,
        "Undefined variable '" + name.lexeme + "'."))

  private def checkNumberOperand(operator: Token, operand: Any): Double =
    operand match
      case d: Double => d
      case _ => throw new RuntimeError(operator, "Operand must be a number.")

  private def checkNumberOperands(operator: Token, left: Any, right: Any): (Double, Double) =
    (left, right) match
      case (dl: Double, dr: Double) => (dl, dr)
      case _ => throw new RuntimeError(operator, "Operands must be numbers.")

  private def isTruthy(obj: Any): Boolean = obj match
    case null => false
    case bool: Boolean => bool
    case _ => true

  //  override def visitCommaExpr(expr: Expr.Comma): Any =
  //    evaluate(expr.left)
  //    evaluate(expr.right)

  override def visitTernaryExpr(expr: Expr.Ternary): Any =
    if isTruthy(evaluate(expr.condition)) then
      evaluate(expr.positiveExpression)
    else evaluate(expr.negativeExpression)

  override def visitBreakStmt(stmt: Stmt.Break.type): Any =
    throw new LoopBreakerException

  override def visitEmptyStmt(stmt: Stmt.Empty.type): Any = {}

  override def visitFunctionExpr(expr: Expr.Function): Any =
    new LoxFunction(null, expr, env, false)

  private def define(name: Token, value: Any): Unit =
    if (env != null) env.define(value)
    else globals(name.lexeme) = value

  private def assign(name: Token, slot: Int, value: Any): Unit =
    if (env != null) env.assign(name, slot, value)
    else globals(name.lexeme) = value

object Interpreter:
  def stringify(obj: Any): String = obj match
    case null => "nil"
    case _: Double => obj.toString.stripSuffix(".0")
    case _ => obj.toString

