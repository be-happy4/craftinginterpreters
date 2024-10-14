package com.craftinginterpreters.scala.lox


import com.craftinginterpreters.scala.lox.Resolver.VariableState.{DECLARED, DEFINED}
import com.craftinginterpreters.scala.lox.Resolver.{Variable, VariableState}
import com.craftinginterpreters.scala.lox.Stmt.Block

import scala.collection.mutable
import scala.collection.mutable.ArrayDeque


object Resolver: //> function-type
  private enum FunctionType:
    case NONE
    /* Resolving and Binding function-type < Classes function-type-method
        case FUNCTION
    */
    case FUNCTION //> function-type-initializer
    case INITIALIZER //< function-type-initializer
    case METHOD

  private enum ClassType:
    case NONE /* Classes class-type < Inheritance class-type-subclass
    CLASS
 */
    case CLASS
    case SUBCLASS

  enum VariableState:
    case DECLARED
    case DEFINED
    case READ

  private case class Variable(
    name: Token,
    var state: VariableState,
    slot: Int)


class Resolver(private val interpreter: Interpreter) extends Expr.Visitor[Unit]:
  private val scopes = new mutable.ArrayDeque[mutable.HashMap[String, Variable]]
  private var currentFunction = Resolver.FunctionType.NONE
  private var currentClass = Resolver.ClassType.NONE

  def resolve(statements: List[Stmt]): Unit =
    for (statement <- statements) {
      resolve(statement)
    }

  private def resolve(stmt: Stmt): Unit =
    stmt.accept(this)

  override def visitBlockStmt(stmt: Stmt.Block): Unit =
    beginScope()
    resolve(stmt.statements)
    endScope()

  override def visitClassStmt(stmt: Stmt.Class): Unit =
    val enclosingClass = currentClass
    currentClass = Resolver.ClassType.CLASS
    declare(stmt.name, VariableState.DEFINED)
    if (stmt.superclass != null && stmt.name.lexeme.equals(stmt.superclass.name.lexeme))
      Lox.error(stmt.superclass.name, "A class can't inherit from itself.")
    if (stmt.superclass != null) {
      currentClass = Resolver.ClassType.SUBCLASS
      resolve(stmt.superclass)
      beginScope()
      declare(Token.SUPER, VariableState.READ)
    }
    beginScope()
    declare(Token.THIS, VariableState.READ)
    for (method <- stmt.methods) {
      var declaration = Resolver.FunctionType.METHOD
      if (method.name.lexeme.equals(TokenType.THIS.key)) declaration = Resolver.FunctionType.INITIALIZER
      resolveFunction(method.function, declaration) // [local]
    }
    endScope()
    if (stmt.superclass != null) endScope()
    currentClass = enclosingClass

  override def visitFunctionStmt(stmt: Stmt.Function): Unit =
    declare(stmt.name)
    define(stmt.name)
    /* Resolving and Binding visit-function-stmt < Resolving and Binding pass-function-type
        resolveFunction(stmt);
    */
    resolveFunction(stmt.function, Resolver.FunctionType.FUNCTION)

  override def visitIfStmt(stmt: Stmt.If): Unit =
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if (stmt.elseBranch != null) resolve(stmt.elseBranch)

  override def visitPrintStmt(stmt: Stmt.Print): Unit =
    resolve(stmt.expression)

  override def visitReturnStmt(stmt: Stmt.Return): Unit =
    if (currentFunction == Resolver.FunctionType.NONE)
      Lox.error(stmt.keyword, "Can't return from top-level code.")
    if (stmt.value != null) {
      if (currentFunction == Resolver.FunctionType.INITIALIZER)
        Lox.error(stmt.keyword, "Can't return a value from an initializer.")
      resolve(stmt.value)
    }

  override def visitVarStmt(stmt: Stmt.Var): Unit =
    declare(stmt.name)
    if (stmt.initializer != null) resolve(stmt.initializer)
    define(stmt.name)

  override def visitWhileStmt(stmt: Stmt.While): Unit =
    resolve(stmt.condition)
    resolve(stmt.body)

  override def visitBreakStmt(stmt: Stmt.Break.type): Unit = {}

  override def visitEmptyStmt(stmt: Stmt.Empty.type): Unit = {}

  override def visitAssignExpr(expr: Expr.Assign): Unit =
    resolve(expr.value)
    resolveLocal(expr, expr.name, false)

  override def visitBinaryExpr(expr: Expr.Binary): Unit =
    resolve(expr.left)
    resolve(expr.right)

  override def visitCallExpr(expr: Expr.Call): Unit =
    resolve(expr.callee)
    for (argument <- expr.arguments) {
      resolve(argument)
    }

  override def visitGetExpr(expr: Expr.Get): Unit =
    resolve(expr.obj)

  override def visitGroupingExpr(expr: Expr.Grouping): Unit =
    resolve(expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): Unit = {}

  override def visitLogicalExpr(expr: Expr.Logical): Unit =
    resolve(expr.left)
    resolve(expr.right)

  override def visitSetExpr(expr: Expr.Set): Unit =
    resolve(expr.value)
    resolve(expr.obj)

  override def visitSuperExpr(expr: Expr.Super): Unit =
    if (currentClass == Resolver.ClassType.NONE)
      Lox.error(expr.keyword, "Can't use 'super' outside of a class.")
    else if (currentClass ne Resolver.ClassType.SUBCLASS)
      Lox.error(expr.keyword, "Can't use 'super' in a class with no superclass.")
    resolveLocal(expr, expr.keyword, true)

  override def visitThisExpr(expr: Expr.This): Unit =
    if (currentClass == Resolver.ClassType.NONE) then
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.")
    else
      resolveLocal(expr, expr.keyword, true)

  override def visitUnaryExpr(expr: Expr.Unary): Unit =
    resolve(expr.right)

  override def visitVariableExpr(expr: Expr.Variable): Unit =
    if (scopes.nonEmpty && (scopes.head.get(expr.name.lexeme) match
      case Some(scope) => scope.state == DECLARED
      case None => false))
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name, true)

  override def visitTernaryExpr(expr: Expr.Ternary): Unit =
    resolve(expr.condition)
    resolve(expr.positiveExpression)
    resolve(expr.negativeExpression)

  override def visitFunctionExpr(expr: Expr.Function): Unit =
    resolveFunction(expr, Resolver.FunctionType.FUNCTION)
  
  /* Resolving and Binding resolve-function < Resolving and Binding set-current-function
    private void resolveFunction(Stmt.Function function) {
  */
  private def resolveFunction(function: Expr.Function, typ: Resolver.FunctionType): Unit =
    val enclosingFunction = currentFunction
    currentFunction = typ
    beginScope()
    for (param <- function.params) {
      declare(param)
      define(param)
    }
    function.body match
      case Block(stmts) => resolve(stmts)
      case stmt => resolve(stmt)
    endScope()
    currentFunction = enclosingFunction

  private def beginScope(): Unit =
    scopes += mutable.HashMap()

  private def endScope(): Unit =
    val scope = scopes.removeLast()
    for ((_, variable) <- scope if variable.state == DEFINED) {
      Lox.error(variable.name, "Local variable is not used.");
    }

  private def declare(name: Token, state: VariableState = VariableState.DECLARED): Unit =
    if (scopes.isEmpty) return
    val scope = scopes.head
    if (scope.contains(name.lexeme))
      Lox.error(name, "Already a variable with this name in this scope.")
    scope(name.lexeme) = Variable(name, state, scope.size)

  private def define(name: Token): Unit =
    if (scopes.isEmpty) return
    scopes.head(name.lexeme).state = VariableState.DEFINED

  private def resolveLocal(expr: Expr, name: Token, isRead: Boolean): Unit =
    scopes.indices.reverse.takeWhile(i =>
      scopes(i).get(name.lexeme) match
        case Some(v) =>
          interpreter.resolve(expr, scopes.size - 1 - i, v.slot)
          if (isRead)
            v.state = VariableState.READ;
          false
        case None => true
    )

