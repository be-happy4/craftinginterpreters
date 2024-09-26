package com.craftinginterpreters.scala.lox


//> Functions import-array-list

import com.craftinginterpreters.scala.lox.Interpreter.stringify
import com.craftinginterpreters.scala.lox.TokenType.*

import scala.collection.mutable
//< Statements and State import-list
//> Resolving and Binding import-map

//< Resolving and Binding import-map//< Resolving and Binding import-map

/* Evaluating Expressions interpreter-class < Statements and State interpreter
class Interpreter implements Expr.Visitor<Object> {
*/
//> Statements and State interpreter
//< Statements and State environment-field
//> Functions interpreter-constructor
//< Resolving and Binding locals-field
//> Statements and State environment-field
class Interpreter extends Expr.Visitor[Any] with Stmt.Visitor[Any]:

  //< Statements and State interpreter
  /* Statements and State environment-field < Functions global-environment
    private Environment environment = new Environment();
  */
  //> Functions global-environment
  private final val globals = new Environment()
  private var environment = globals
  //< Functions global-environment
  //> Resolving and Binding locals-field
  final private val locals = new mutable.HashMap[Expr, Int]()

  globals.define("clock", new LoxCallable() {
    override def arity = 0

    override def call(interpreter: Interpreter, arguments: List[Any]): Double =
      System.currentTimeMillis / 1000.0

    override def toString = "<native fn>"
  })

  //< Functions interpreter-constructor
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
  //> Statements and State interpret
  def interpret(statements: List[Stmt]): Any =
    try {
      statements.map(execute)
    } catch {
      case error: RuntimeError =>
        Lox.runtimeError(error)
    }

  //< Statements and State interpret
  //> evaluate
  private def evaluate(expr: Expr): Any = expr.accept(this)

  //< evaluate
  //> Statements and State execute
  private def execute(stmt: Stmt): Any =
    stmt.accept(this)

  //< Statements and State execute
  //> Resolving and Binding resolve
  def resolve(expr: Expr, depth: Int): Unit =
    locals.put(expr, depth)

  //< Resolving and Binding resolve
  //> Statements and State execute-block
  def executeBlock(statements: List[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try {
      this.environment = environment
      for (statement <- statements) {
        execute(statement)
      }
    } finally this.environment = previous

  //< Statements and State execute-block
  //> Statements and State visit-block
  override def visitBlockStmt(stmt: Stmt.Block): Unit =
    executeBlock(stmt.statements, new Environment(environment))

  //< Statements and State visit-block
  //> Classes interpreter-visit-class
  override def visitClassStmt(stmt: Stmt.Class): Unit =
    //> Inheritance interpret-superclass
    var superclass: Any = null
    if (stmt.superclass != null) {
      superclass = evaluate(stmt.superclass)
      if (!superclass.isInstanceOf[LoxClass])
        throw new RuntimeError(stmt.superclass.name, "Superclass must be a class.")
    }
    //< Inheritance interpret-superclass
    environment.define(stmt.name.lexeme, null)
    //> Inheritance begin-superclass-environment
    if (stmt.superclass != null) {
      environment = new Environment(environment)
      environment.define("super", superclass)
    }
    //< Inheritance begin-superclass-environment
    //> interpret-methods
    val methods = new mutable.HashMap[String, LoxFunction]
    for (method <- stmt.methods) {
      /* Classes interpret-methods < Classes interpreter-method-initializer
            LoxFunction function = new LoxFunction(method, environment);
      */
      //> interpreter-method-initializer
      val function = new LoxFunction(method, environment, method.name.lexeme.equals("init"))
      //< interpreter-method-initializer
      methods.put(method.name.lexeme, function)
    }
    /* Classes interpret-methods < Inheritance interpreter-construct-class
        LoxClass klass = new LoxClass(stmt.name.lexeme, methods);
    */
    //> Inheritance interpreter-construct-class
    val klass = new LoxClass(stmt.name.lexeme, superclass.asInstanceOf[LoxClass], methods.toMap)
    //> end-superclass-environment
    if (superclass != null) environment = environment.enclosing
    //< end-superclass-environment
    //< Inheritance interpreter-construct-class
    //< interpret-methods
    /* Classes interpreter-visit-class < Classes interpret-methods
        LoxClass klass = new LoxClass(stmt.name.lexeme);
    */
    environment.assign(stmt.name, klass)

  //< Classes interpreter-visit-class
  //> Statements and State visit-expression-stmt
  override def visitExpressionStmt(stmt: Stmt.Expression): Any =
    evaluate(stmt.expression)

  //< Statements and State visit-expression-stmt
  //> Functions visit-function
  override def visitFunctionStmt(stmt: Stmt.Function): Unit =
    /* Functions visit-function < Functions visit-closure
        LoxFunction function = new LoxFunction(stmt);
    */
    /* Functions visit-closure < Classes construct-function
        LoxFunction function = new LoxFunction(stmt, environment);
    */
    //> Classes construct-function
    val function = new LoxFunction(stmt, environment, false)
    //< Classes construct-function
    environment.define(stmt.name.lexeme, function)

  //< Functions visit-function
  //> Control Flow visit-if
  override def visitIfStmt(stmt: Stmt.If): Unit =
    if (isTruthy(evaluate(stmt.condition))) execute(stmt.thenBranch)
    else if (stmt.elseBranch != null) execute(stmt.elseBranch)

  //< Control Flow visit-if
  //> Statements and State visit-print
  override def visitPrintStmt(stmt: Stmt.Print): Unit =
    val value = evaluate(stmt.expression)
    println(stringify(value))

  //< Statements and State visit-print
  //> Functions visit-return
  override def visitReturnStmt(stmt: Stmt.Return): Unit =
    var value: Any = null
    if (stmt.value != null) value = evaluate(stmt.value)
    throw new Return(value)

  //< Functions visit-return
  //> Statements and State visit-var
  override def visitVarStmt(stmt: Stmt.Var): Unit =
    var value: Any = Token.dummy
    if (stmt.initializer != null) value = evaluate(stmt.initializer)
    environment.define(stmt.name.lexeme, value)

  //< Statements and State visit-var
  //> Control Flow visit-while
  override def visitWhileStmt(stmt: Stmt.While): Unit =
    while (isTruthy(evaluate(stmt.condition))) execute(stmt.body)

  //< Control Flow visit-while
  //> Statements and State visit-assign
  override def visitAssignExpr(expr: Expr.Assign): Any =
    val value = evaluate(expr.value)
    /* Statements and State visit-assign < Resolving and Binding resolved-assign
        environment.assign(expr.name, value);
    */
    //> Resolving and Binding resolved-assign
    locals.get(expr) match
      case Some(distance) => environment.assignAt(distance, expr.name, value)
      case None => globals.assign(expr.name, value)
    //< Resolving and Binding resolved-assign
    value

  //< Statements and State visit-assign
  //> visit-binary
  override def visitBinaryExpr(expr: Expr.Binary): Any =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right) // [left]

    expr.operator.typ match
      //> binary-equality
      case BANG_EQUAL =>
        !isEqual(left, right)
      case EQUAL_EQUAL =>
        isEqual(left, right)
      //< binary-equality
      //> binary-comparison
      case GREATER =>
        //> check-greater-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-greater-operand
        dl > dr
      case GREATER_EQUAL =>
        //> check-greater-equal-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-greater-equal-operand
        dl >= dr
      case LESS =>
        //> check-less-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-less-operand
        dl < dr
      case LESS_EQUAL =>
        //> check-less-equal-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-less-equal-operand
        dl <= dr
      //< binary-comparison
      case MINUS =>
        //> check-minus-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-minus-operand
        dl - dr
      //> binary-plus
      case PLUS =>
        (left, right) match
          case (dl: Double, dr: Double) => dl + dr
          case (dl: String, _) => dl + stringify(right)
          case (_, dr: String) => stringify(left) + dr
          case _ => throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      //< string-wrong-type
      //< binary-plus
      case SLASH =>
        //> check-slash-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-slash-operand
        dl / dr
      case STAR =>
        //> check-star-operand
        val (dl, dr) = checkNumberOperands(expr.operator, left, right)
        //< check-star-operand
        dl * dr
      case _ => throw MatchError(s"${expr.operator}")

  //< visit-binary
  //> Functions visit-call
  override def visitCallExpr(expr: Expr.Call): Any =
    val callee = evaluate(expr.callee)
    val arguments = new mutable.ListBuffer[Any]()
    for (argument <- expr.arguments) { // [in-order]
      arguments += evaluate(argument)
    }
    callee match
      //> check-is-callable
      case function: LoxCallable =>
        //> check-arity
        if (arguments.size ne function.arity) throw new RuntimeError(expr.paren,
          s"Expected ${function.arity} arguments but got ${arguments.size}.")
        //< check-arity
        function.call(this, arguments.toList)
      case _ => throw new RuntimeError(expr.paren, "Can only call functions and classes.")

  //< Functions visit-call
  //> Classes interpreter-visit-get
  override def visitGetExpr(expr: Expr.Get): Any = evaluate(expr.obj) match
    case value: LoxInstance => value.get(expr.name)
    case _ => throw new RuntimeError(expr.name, "Only instances have properties.")

  //< Classes interpreter-visit-get
  //> visit-grouping
  override def visitGroupingExpr(expr: Expr.Grouping): Any = evaluate(expr.expression)

  //< visit-grouping
  //> visit-literal
  override def visitLiteralExpr(expr: Expr.Literal): Any = expr.value

  //< visit-literal
  //> Control Flow visit-logical
  override def visitLogicalExpr(expr: Expr.Logical): Any =
    val left = evaluate(expr.left)
    if (expr.operator.typ eq TokenType.OR) if (isTruthy(left)) return left
    else if (!isTruthy(left)) return left
    evaluate(expr.right)

  //< Control Flow visit-logical
  //> Classes interpreter-visit-set
  override def visitSetExpr(expr: Expr.Set): Any =
    val obj = evaluate(expr.obj)
    obj match
      case inst: LoxInstance =>
        val value = evaluate(expr.value)
        inst.set(expr.name, value)
        value
      case _ =>
        throw new RuntimeError(expr.name, "Only instances have fields.")

  //< Classes interpreter-visit-set
  //> Inheritance interpreter-visit-super
  override def visitSuperExpr(expr: Expr.Super): Any =
    val distance = locals(expr)
    val superclass = environment.getAt(distance, "super").asInstanceOf[LoxClass]
    //> super-find-this
    val obj = environment.getAt(distance - 1, "this").asInstanceOf[LoxInstance]
    //< super-find-this
    //> super-find-method
    val method = superclass.findMethod(expr.method.lexeme)
    //> super-no-method
    if (method == null) throw new RuntimeError(expr.method, "Undefined property '" + expr.method.lexeme + "'.")
    //< super-no-method
    method.bind(obj)
  //< super-find-method

  //< Inheritance interpreter-visit-super
  //> Classes interpreter-visit-this
  override def visitThisExpr(expr: Expr.This): Any = lookUpVariable(expr.keyword, expr)

  //< Classes interpreter-visit-this
  //> visit-unary
  override def visitUnaryExpr(expr: Expr.Unary): Any =
    val right = evaluate(expr.right)
    expr.operator.typ match
      //> unary-bang
      case BANG =>
        !isTruthy(right)
      //< unary-bang
      case MINUS =>
        //> check-unary-operand
        checkNumberOperand(expr.operator, right)
        //< check-unary-operand
        -right.asInstanceOf[Double]
      // Unreachable.
      case _ => null

  //< visit-unary
  //> Statements and State visit-variable
  override def visitVariableExpr(expr: Expr.Variable): Any =
    /* Statements and State visit-variable < Resolving and Binding call-look-up-variable
        return environment.get(expr.name);
    */
    //> Resolving and Binding call-look-up-variable
    lookUpVariable(expr.name, expr)
  //< Resolving and Binding call-look-up-variable

  //> Resolving and Binding look-up-variable
  private def lookUpVariable(name: Token, expr: Expr): Any =
    val opt = locals.get(expr) match
      case Some(distance) => environment.getAt(distance, name.lexeme)
      case None => globals.get(name)
    opt match
      case Some(v) =>
        if v == Token.dummy then
          throw new RuntimeError(name, "Variable '" + name.lexeme + "' not initialized.")
        v
      case None => throw new RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")

  //< Resolving and Binding look-up-variable
  //< Statements and State visit-variable
  //> check-operand
  private def checkNumberOperand(operator: Token, operand: Any): Double =
    operand match
      case d: Double => d
      case _ => throw new RuntimeError(operator, "Operand must be a number.")

  //< check-operand
  //> check-operands
  private def checkNumberOperands(operator: Token, left: Any, right: Any): (Double, Double) =
    (left, right) match
      case (dl: Double, dr: Double) => (dl, dr)
      case _ => throw new RuntimeError(operator, "Operands must be numbers.")

  //< check-operands
  //> is-truthy
  private def isTruthy(obj: Any): Boolean = obj match
    case null => false
    case bool: Boolean => bool
    case _ => true

  //< is-truthy
  //> is-equal
  private def isEqual(a: Any, b: Any): Boolean =
    if a == null then b == null else a.equals(b)

  override def visitCommaExpr(expr: Expr.Comma): Any =
    evaluate(expr.left)
    evaluate(expr.right)

  override def visitTernaryExpr(expr: Expr.Ternary): Any =
    if isTruthy(evaluate(expr.condition)) then
      evaluate(expr.positiveExpression)
    else evaluate(expr.negativeExpression)
//< stringify

object Interpreter:
  //< is-equal
  //> stringify
  def stringify(obj: Any): String = obj match
    case null => "nil"
    case _: Double => obj.toString.stripSuffix(".0")
    case _ => obj.toString

