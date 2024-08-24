//> Appendix II stmt

package com.craftinginterpreters
package scala.lox

import _root_.scala.collection.immutable.List

sealed abstract class Stmt:
  def accept[R](visitor: Stmt.Visitor[R]): R =
    this match
      case x: Stmt.Block => visitor.visitBlockStmt(x)
      case x: Stmt.Class => visitor.visitClassStmt(x)
      case x: Stmt.Expression => visitor.visitExpressionStmt(x)
      case x: Stmt.Function => visitor.visitFunctionStmt(x)
      case x: Stmt.If => visitor.visitIfStmt(x)
      case x: Stmt.Print => visitor.visitPrintStmt(x)
      case x: Stmt.Return => visitor.visitReturnStmt(x)
      case x: Stmt.Var => visitor.visitVarStmt(x)
      case x: Stmt.While => visitor.visitWhileStmt(x)

object Stmt:
  trait Visitor[R]:
    def visitBlockStmt(stmt: Stmt.Block): R
    def visitClassStmt(stmt: Stmt.Class): R
    def visitExpressionStmt(stmt: Stmt.Expression): R
    def visitFunctionStmt(stmt: Stmt.Function): R
    def visitIfStmt(stmt: Stmt.If): R
    def visitPrintStmt(stmt: Stmt.Print): R
    def visitReturnStmt(stmt: Stmt.Return): R
    def visitVarStmt(stmt: Stmt.Var): R
    def visitWhileStmt(stmt: Stmt.While): R

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
//> stmt-expression
  class Expression(val expression: Expr) extends Stmt
//< stmt-expression
//> stmt-function
  class Function(
    val name: Token,
    val params: List[Token],
    val body: List[Stmt]) extends Stmt
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

//< Appendix II stmt
