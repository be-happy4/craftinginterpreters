package com.craftinginterpreters.scala.lox


import com.craftinginterpreters.scala.lox.Resolver.VariableState.{DECLARED, DEFINED}
import com.craftinginterpreters.scala.lox.Resolver.{Variable, VariableState}

import scala.collection.mutable
import scala.collection.mutable.ArrayDeque


object Resolver: //> function-type
  private enum FunctionType:
    case NONE
    /* Resolving and Binding function-type < Classes function-type-method
        case FUNCTION
    */
    //> Classes function-type-method
    case FUNCTION //> function-type-initializer
    case INITIALIZER //< function-type-initializer
    case METHOD
  //< Classes function-type-method

  //< function-type
  //> Classes class-type
  private enum ClassType:
    case NONE /* Classes class-type < Inheritance class-type-subclass
    CLASS
 */
    //> Inheritance class-type-subclass
    case CLASS
    case SUBCLASS
//< Inheritance class-type-subclass

  enum VariableState:
    case DECLARED
    case DEFINED
    case READ

  private case class Variable(
    name: Token,
    var state: VariableState)

  private final val USED_VARIABLE = Variable(Token.DUMMY, VariableState.READ)

class Resolver(private val interpreter: Interpreter) extends Expr.Visitor[Unit]
  with Stmt.Visitor[Unit]:
  //> scopes-field
  private val scopes = new mutable.ArrayDeque[mutable.HashMap[String, Variable]]
  //< scopes-field
  //> function-type-field
  private var currentFunction = Resolver.FunctionType.NONE
  private var currentClass = Resolver.ClassType.NONE

  //< Classes class-type
  //> resolve-statements
  def resolve(statements: List[Stmt]): Unit = {
    for (statement <- statements) {
      resolve(statement)
    }
  }

  //< resolve-statements
  //> visit-block-stmt
  override def visitBlockStmt(stmt: Stmt.Block): Unit =
    beginScope()
    resolve(stmt.statements)
    endScope()

  //< visit-block-stmt
  //> Classes resolver-visit-class
  override def visitClassStmt(stmt: Stmt.Class): Unit =
    //> set-current-class
    val enclosingClass = currentClass
    currentClass = Resolver.ClassType.CLASS
    //< set-current-class
    declare(stmt.name)
    define(stmt.name)
    //> Inheritance resolve-superclass
    //> inherit-self
    if (stmt.superclass != null && stmt.name.lexeme.equals(stmt.superclass.name.lexeme))
      Lox.error(stmt.superclass.name, "A class can't inherit from itself.")
    //< inherit-self
    if (stmt.superclass != null) {
      //> set-current-subclass
      currentClass = Resolver.ClassType.SUBCLASS
      //< set-current-subclass
      resolve(stmt.superclass)
    }
    //< Inheritance resolve-superclass
    //> Inheritance begin-super-scope
    if (stmt.superclass != null) {
      beginScope()
      Token.UNINITIATED
      scopes.head("super") = Resolver.USED_VARIABLE
    }
    //< Inheritance begin-super-scope
    //> resolve-methods
    //> resolver-begin-this-scope
    beginScope()
    scopes.head("this") = Resolver.USED_VARIABLE
    //< resolver-begin-this-scope
    for (method <- stmt.methods) {
      var declaration = Resolver.FunctionType.METHOD
      //> resolver-initializer-type
      if (method.name.lexeme.equals("init")) declaration = Resolver.FunctionType.INITIALIZER
      //< resolver-initializer-type
      resolveFunction(method.function, declaration) // [local]

    }
    //> resolver-end-this-scope
    endScope()
    //< resolver-end-this-scope
    //< resolve-methods
    //> Inheritance end-super-scope
    if (stmt.superclass != null) endScope()
    //< Inheritance end-super-scope
    //> restore-current-class
    currentClass = enclosingClass
  //< restore-current-class

  //< Classes resolver-visit-class
  //> visit-expression-stmt
  override def visitExprStmt(stmt: Expr): Unit =
    resolve(stmt)

  //< visit-expression-stmt
  //> visit-function-stmt
  override def visitFunctionStmt(stmt: Stmt.Function): Unit =
    declare(stmt.name)
    define(stmt.name)
    /* Resolving and Binding visit-function-stmt < Resolving and Binding pass-function-type
        resolveFunction(stmt);
    */
    //> pass-function-type
    resolveFunction(stmt.function, Resolver.FunctionType.FUNCTION)
    //< pass-function-type

  //< visit-function-stmt
  //> visit-if-stmt
  override def visitIfStmt(stmt: Stmt.If): Unit =
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if (stmt.elseBranch != null) resolve(stmt.elseBranch)

  //< visit-if-stmt
  //> visit-print-stmt
  override def visitPrintStmt(stmt: Stmt.Print): Unit =
    resolve(stmt.expression)

  //< visit-print-stmt
  //> visit-return-stmt
  override def visitReturnStmt(stmt: Stmt.Return): Unit =
    //> return-from-top
    if (currentFunction == Resolver.FunctionType.NONE)
      Lox.error(stmt.keyword, "Can't return from top-level code.")
    //< return-from-top
    if (stmt.value != null) {
      //> Classes return-in-initializer
      if (currentFunction == Resolver.FunctionType.INITIALIZER) Lox.error(stmt.keyword, "Can't return a value from an initializer.")
      //< Classes return-in-initializer
      resolve(stmt.value)
    }

  //< visit-return-stmt
  //> visit-var-stmt
  override def visitVarStmt(stmt: Stmt.Var): Unit =
    declare(stmt.name)
    if (stmt.initializer != null) resolve(stmt.initializer)
    define(stmt.name)

  //< visit-var-stmt
  //> visit-while-stmt
  override def visitWhileStmt(stmt: Stmt.While): Unit =
    resolve(stmt.condition)
    resolve(stmt.body)

  override def visitBreakStmt(stmt: Stmt.Break.type): Unit = {}

  override def visitEmptyStmt(stmt: Stmt.Empty.type): Unit = {}

  //< visit-while-stmt
  //> visit-assign-expr
  override def visitAssignExpr(expr: Expr.Assign): Unit =
    resolve(expr.value)
    resolveLocal(expr, expr.name, false)

  //< visit-assign-expr
  //> visit-binary-expr
  override def visitBinaryExpr(expr: Expr.Binary): Unit =
    resolve(expr.left)
    resolve(expr.right)

  //< visit-binary-expr
  //> visit-call-expr
  override def visitCallExpr(expr: Expr.Call): Unit =
    resolve(expr.callee)
    for (argument <- expr.arguments) {
      resolve(argument)
    }

  //< visit-call-expr
  //> Classes resolver-visit-get
  override def visitGetExpr(expr: Expr.Get): Unit =
    resolve(expr.obj)

  //< Classes resolver-visit-get
  //> visit-grouping-expr
  override def visitGroupingExpr(expr: Expr.Grouping): Unit =
    resolve(expr.expression)

  //< visit-grouping-expr
  //> visit-literal-expr
  override def visitLiteralExpr(expr: Expr.Literal): Unit = {}

  //< visit-literal-expr
  //> visit-logical-expr
  override def visitLogicalExpr(expr: Expr.Logical): Unit =
    resolve(expr.left)
    resolve(expr.right)

  //< visit-logical-expr
  //> Classes resolver-visit-set
  override def visitSetExpr(expr: Expr.Set): Unit =
    resolve(expr.value)
    resolve(expr.obj)

  //< Classes resolver-visit-set
  //> Inheritance resolve-super-expr
  override def visitSuperExpr(expr: Expr.Super): Unit =
    //> invalid-super
    if (currentClass == Resolver.ClassType.NONE)
      Lox.error(expr.keyword, "Can't use 'super' outside of a class.")
    else if (currentClass ne Resolver.ClassType.SUBCLASS)
      Lox.error(expr.keyword, "Can't use 'super' in a class with no superclass.")
    //< invalid-super
    resolveLocal(expr, expr.keyword, true)

  //< Inheritance resolve-super-expr
  //> Classes resolver-visit-this
  override def visitThisExpr(expr: Expr.This): Unit =
    //> this-outside-of-class
    if (currentClass == Resolver.ClassType.NONE) then
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.")
    else
      //< this-outside-of-class
      resolveLocal(expr, expr.keyword, true)

  //< Classes resolver-visit-this
  //> visit-unary-expr
  override def visitUnaryExpr(expr: Expr.Unary): Unit =
    resolve(expr.right)

  //< visit-unary-expr
  //> visit-variable-expr
  override def visitVariableExpr(expr: Expr.Variable): Unit =
    if (scopes.nonEmpty && (scopes.head.get(expr.name.lexeme) match
      case Some(scope) => scope.state == DECLARED
      case None => false))
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name, true)

//  override def visitCommaExpr(expr: Expr.Comma): Unit =
//    resolve(expr.left)
//    resolve(expr.right)

  override def visitTernaryExpr(expr: Expr.Ternary): Unit =
    resolve(expr.condition)
    resolve(expr.positiveExpression)
    resolve(expr.negativeExpression)

  override def visitFunctionExpr(expr: Expr.Function): Unit =
    resolveFunction(expr, Resolver.FunctionType.FUNCTION)
  
  //< visit-variable-expr
  //> resolve-stmt
  private def resolve(stmt: Stmt): Unit =
    stmt.accept(this)

  //< resolve-stmt
  //> resolve-expr
  private def resolve(expr: Expr): Unit =
    expr.accept(this)

  //< resolve-expr
  //> resolve-function
  /* Resolving and Binding resolve-function < Resolving and Binding set-current-function
    private void resolveFunction(Stmt.Function function) {
  */
  //> set-current-function
  private def resolveFunction(function: Expr.Function, typ: Resolver.FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = typ
    //< set-current-function
    beginScope()
    for (param <- function.params) {
      declare(param)
      define(param)
    }
    resolve(function.body)
    endScope()
    //> restore-current-function
    currentFunction = enclosingFunction
    //< restore-current-function

  //< resolve-function
  //> begin-scope
  private def beginScope(): Unit =
    scopes += mutable.HashMap()

  //< begin-scope
  //> end-scope
  private def endScope(): Unit =
    val scope = scopes.removeLast()
    for ((_, variable) <- scope if variable.state == DEFINED) {
      Lox.error(variable.name, "Local variable is not used.");
    }

  //< end-scope
  //> declare
  private def declare(name: Token): Unit =
    if (scopes.isEmpty) return
    val scope = scopes.head
    //> duplicate-variable
    if (scope.contains(name.lexeme))
      Lox.error(name, "Already a variable with this name in this scope.")
    //< duplicate-variable
    scope(name.lexeme) = Variable(name, VariableState.DECLARED)

  //< declare
  //> define
  private def define(name: Token): Unit =
    if (scopes.isEmpty) return
    scopes.head(name.lexeme).state = VariableState.DEFINED

  //< define
  //> resolve-local
  private def resolveLocal(expr: Expr, name: Token, isRead: Boolean): Unit =
    scopes.indices.reverse.takeWhile(i =>
      scopes(i).get(name.lexeme) match
        case Some(v) =>
          interpreter.resolve(expr, scopes.size - 1 - i)
          if (isRead)
            v.state = VariableState.READ;
          false
        case None => true
    )
//< resolve-local

