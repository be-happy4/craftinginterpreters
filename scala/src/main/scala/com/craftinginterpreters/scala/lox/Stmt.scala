
package com.craftinginterpreters.scala.lox

import scala.collection.immutable.List

sealed abstract class Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): R =
    this match
      case x: Stmt.Block => visitor.visitBlockStmt(x)
      case x: Stmt.Class => visitor.visitClassStmt(x)
      case x: Expr => visitor.visitExprStmt(x)
      case x: Stmt.Function => visitor.visitFunctionStmt(x)
      case x: Stmt.If => visitor.visitIfStmt(x)
      case x: Stmt.Print => visitor.visitPrintStmt(x)
      case x: Stmt.Return => visitor.visitReturnStmt(x)
      case x: Stmt.Var => visitor.visitVarStmt(x)
      case x: Stmt.While => visitor.visitWhileStmt(x)
      case x: Stmt.Break.type => visitor.visitBreakStmt(x)
      case x: Stmt.Empty.type => visitor.visitEmptyStmt(x)

object Stmt:
  trait Visitor[R]:
    def visitBlockStmt(stmt: Stmt.Block): R
    def visitClassStmt(stmt: Stmt.Class): R
    def visitExprStmt(stmt: Expr): R
    def visitFunctionStmt(stmt: Stmt.Function): R
    def visitIfStmt(stmt: Stmt.If): R
    def visitPrintStmt(stmt: Stmt.Print): R
    def visitReturnStmt(stmt: Stmt.Return): R
    def visitVarStmt(stmt: Stmt.Var): R
    def visitWhileStmt(stmt: Stmt.While): R
    def visitBreakStmt(stmt: Stmt.Break.type): R
    def visitEmptyStmt(stmt: Stmt.Empty.type): R

// Nested Stmt classes here...
  case class Block(statements: List[Stmt]) extends Stmt
  case class Class(
    name: Token,
    superclass: Expr.Variable,
    methods: List[Stmt.Function]) extends Stmt
//  case class Expression(val expression: Expr) extends Stmt
  case class Function(name: Token, function: Expr.Function) extends Stmt
  case class If(
    condition: Expr,
    thenBranch: Stmt,
    elseBranch: Stmt) extends Stmt
  case class Print(expression: Expr) extends Stmt
  case class Return(keyword: Token, value: Expr) extends Stmt
  case class Var(name: Token, initializer: Expr) extends Stmt
  case class While(condition: Expr, body: Stmt) extends Stmt
  object Break extends Stmt
  object Empty extends Stmt


sealed abstract class Expr extends Stmt:
  def accept[R](visitor: Expr.Visitor[R]): R =
    this match
      case x: Expr.Assign => visitor.visitAssignExpr(x)
      case x: Expr.Binary => visitor.visitBinaryExpr(x)
      case x: Expr.Call => visitor.visitCallExpr(x)
      case x: Expr.Get => visitor.visitGetExpr(x)
      case x: Expr.Grouping => visitor.visitGroupingExpr(x)
      case x: Expr.Literal => visitor.visitLiteralExpr(x)
      case x: Expr.Logical => visitor.visitLogicalExpr(x)
      case x: Expr.Set => visitor.visitSetExpr(x)
      case x: Expr.Super => visitor.visitSuperExpr(x)
      case x: Expr.This => visitor.visitThisExpr(x)
      case x: Expr.Unary => visitor.visitUnaryExpr(x)
      case x: Expr.Variable => visitor.visitVariableExpr(x)
      case x: Expr.Ternary => visitor.visitTernaryExpr(x)
      case x: Expr.Function => visitor.visitFunctionExpr(x)

object Expr:
  trait Visitor[R] extends Stmt.Visitor[R]:
    override def visitExprStmt(stmt: Expr): R = stmt.accept(Visitor.this)
    def visitAssignExpr(expr: Expr.Assign): R
    def visitBinaryExpr(expr: Expr.Binary): R
    def visitCallExpr(expr: Expr.Call): R
    def visitGetExpr(expr: Expr.Get): R
    def visitGroupingExpr(expr: Expr.Grouping): R
    def visitLiteralExpr(expr: Expr.Literal): R
    def visitLogicalExpr(expr: Expr.Logical): R
    def visitSetExpr(expr: Expr.Set): R
    def visitSuperExpr(expr: Expr.Super): R
    def visitThisExpr(expr: Expr.This): R
    def visitUnaryExpr(expr: Expr.Unary): R
    def visitVariableExpr(expr: Expr.Variable): R
    def visitTernaryExpr(expr: Expr.Ternary): R
    def visitFunctionExpr(expr: Expr.Function): R

  // Nested Expr classes here...
  case class Assign(name: Token, value: Expr) extends Expr
  case class Binary(
    left: Expr,
    operator: Token,
    right: Expr) extends Expr
  case class Call(
    callee: Expr,
    paren: Token,
    arguments: List[Expr]) extends Expr
  case class Get(obj: Expr, name: Token) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: Any) extends Expr
  case class Logical(
    left: Expr,
    operator: Token,
    right: Expr) extends Expr
  case class Set(
    obj: Expr,
    name: Token,
    value: Expr) extends Expr
  case class Super(keyword: Token, method: Token) extends Expr
  case class This(keyword: Token) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class Variable(name: Token) extends Expr
  case class Ternary(
    condition: Expr,
    positiveExpression: Expr,
    negativeExpression: Expr) extends Expr
  case class Function(params: List[Token], body: Stmt) extends Expr

