//> Appendix II stmt

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
//> stmt-block
  class Block(val statements: List[Stmt]) extends Stmt
//< stmt-block
//> stmt-class
  class Class(
    val name: Token,
    val superclass: Expr.Variable,
    val methods: List[Stmt.Function]) extends Stmt
//< stmt-class
////> stmt-expression
//  class Expression(val expression: Expr) extends Stmt
////< stmt-expression
//> stmt-function
  class Function(val name: Token, val function: Expr.Function) extends Stmt
//< stmt-function
//> stmt-if
  class If(
    val condition: Expr,
    val thenBranch: Stmt,
    val elseBranch: Stmt) extends Stmt
//< stmt-if
//> stmt-print
  class Print(val expression: Expr) extends Stmt
//< stmt-print
//> stmt-return
  class Return(val keyword: Token, val value: Expr) extends Stmt
//< stmt-return
//> stmt-var
  class Var(val name: Token, val initializer: Expr) extends Stmt
//< stmt-var
//> stmt-while
  class While(val condition: Expr, val body: Stmt) extends Stmt
//< stmt-while
//> stmt-break
  object Break extends Stmt
//< stmt-break
  object Empty extends Stmt

//< Appendix II stmt

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
  trait Visitor[R]:
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
  //> expr-assign
  class Assign(val name: Token, val value: Expr) extends Expr
  //< expr-assign
  //> expr-binary
  class Binary(
    val left: Expr,
    val operator: Token,
    val right: Expr) extends Expr
  //< expr-binary
  //> expr-call
  class Call(
    val callee: Expr,
    val paren: Token,
    val arguments: List[Expr]) extends Expr
  //< expr-call
  //> expr-get
  class Get(val obj: Expr, val name: Token) extends Expr
  //< expr-get
  //> expr-grouping
  class Grouping(val expression: Expr) extends Expr
  //< expr-grouping
  //> expr-literal
  class Literal(val value: Any) extends Expr
  //< expr-literal
  //> expr-logical
  class Logical(
    val left: Expr,
    val operator: Token,
    val right: Expr) extends Expr
  //< expr-logical
  //> expr-set
  class Set(
    val obj: Expr,
    val name: Token,
    val value: Expr) extends Expr
  //< expr-set
  //> expr-super
  class Super(val keyword: Token, val method: Token) extends Expr
  //< expr-super
  //> expr-this
  class This(val keyword: Token) extends Expr
  //< expr-this
  //> expr-unary
  class Unary(val operator: Token, val right: Expr) extends Expr
  //< expr-unary
  //> expr-variable
  class Variable(val name: Token) extends Expr
  //< expr-variable
  //> expr-ternary
  class Ternary(
    val condition: Expr,
    val positiveExpression: Expr,
    val negativeExpression: Expr) extends Expr
  //< expr-ternary
  //> expr-function
  class Function(val params: List[Token], val body: Stmt) extends Expr
//< expr-function

//< Appendix II expr
