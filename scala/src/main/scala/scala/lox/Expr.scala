//> Appendix II expr

package com.craftinginterpreters
package scala.lox

import _root_.scala.collection.immutable.List

sealed abstract class Expr:
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

//< Appendix II expr
