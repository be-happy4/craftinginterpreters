package com.craftinginterpreters.scala.lox

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}



/* Representing Code ast-printer < Statements and State omit
class AstPrinter implements Expr.Visitor<String> {
*/
class AstPrinter extends Expr.Visitor[String]:
  def print(stmt: Stmt): String = stmt.accept(this)

  override def visitBlockStmt(stmt: Stmt.Block): String = {
    val builder = new StringBuilder
    builder.append("(block ")
    for (statement <- stmt.statements) {
      builder.append(statement.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  override def visitClassStmt(stmt: Stmt.Class): String = {
    val builder = new StringBuilder
    builder.append("(class " + stmt.name.lexeme)
    if (stmt.superclass != null) builder.append(" < " + print(stmt.superclass))
    for (method <- stmt.methods) {
      builder.append(" " + print(method))
    }
    builder.append(")")
    builder.toString
  }

  override def visitExprStmt(stmt: Expr): String =
    parenthesize(";", stmt)

  override def visitFunctionStmt(stmt: Stmt.Function): String =
    s"""
       |(fun ${stmt.name.lexeme}
       |(${stmt.function.params.map(_.lexeme).mkString(" ")}) 
       |${stmt.function.body.accept(this)})"""
      .stripMargin
      .replace("\n", "")

  override def visitIfStmt(stmt: Stmt.If): String = {
    if (stmt.elseBranch == null) return parenthesize2("if", stmt.condition, stmt.thenBranch)
    parenthesize2("if-else", stmt.condition, stmt.thenBranch, stmt.elseBranch)
  }

  override def visitPrintStmt(stmt: Stmt.Print): String = parenthesize("print", stmt.expression)

  override def visitReturnStmt(stmt: Stmt.Return): String = {
    if (stmt.value == null) return "(return)"
    parenthesize("return", stmt.value)
  }

  override def visitVarStmt(stmt: Stmt.Var): String = {
    if (stmt.initializer == null) return parenthesize2("var", stmt.name)
    parenthesize2("var", stmt.name, "=", stmt.initializer)
  }

  override def visitWhileStmt(stmt: Stmt.While): String = parenthesize2("while", stmt.condition, stmt.body)

  override def visitAssignExpr(expr: Expr.Assign): String = parenthesize2("=", expr.name.lexeme, expr.value)

  override def visitBinaryExpr(expr: Expr.Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitCallExpr(expr: Expr.Call): String = parenthesize2("call", expr.callee, expr.arguments)

  override def visitGetExpr(expr: Expr.Get): String = parenthesize2(".", expr.obj, expr.name.lexeme)

  override def visitGroupingExpr(expr: Expr.Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) return "nil"
    expr.value.toString
  }

  override def visitLogicalExpr(expr: Expr.Logical): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visitSetExpr(expr: Expr.Set): String = parenthesize2("=", expr.obj, expr.name.lexeme, expr.value)

  override def visitSuperExpr(expr: Expr.Super): String = parenthesize2("super", expr.method)

  override def visitThisExpr(expr: Expr.This) = "this"

  override def visitUnaryExpr(expr: Expr.Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  override def visitVariableExpr(expr: Expr.Variable): String = expr.name.lexeme

//  override def visitCommaExpr(expr: Expr.Comma): String =
//    s"(, ${expr.left.accept(this)} ${expr.right.accept(this)})"

  override def visitTernaryExpr(expr: Expr.Ternary): String =
    s"(${expr.condition.accept(this)} ? ${expr.positiveExpression.accept(this)} : " +
      s"${expr.negativeExpression.accept(this)})"

  private def parenthesize(name: String, exprs: Expr*) = {
    val builder = new StringBuilder
    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ")
      builder.append(expr.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  // Note: AstPrinting other types of syntax trees is not shown in the
  // book, but this is provided here as a reference for those reading
  // the full code.
  private def parenthesize2(name: String, parts: AnyRef*) = {
    val builder = new StringBuilder
    builder.append("(").append(name)
    transform(builder, parts*)
    builder.append(")")
    builder.toString
  }

  private def transform(builder: StringBuilder, parts: AnyRef*): Unit =
    for (part <- parts) {
      builder.append(" ")
      part match
        case expr: Expr => builder.append(expr.accept(this))
        case stmt: Stmt => builder.append(stmt.accept(this))
        case token: Token => builder.append(token.lexeme)
        case value: List[_] => transform(builder, value)
        case _ => builder.append(part)
    }

  override def visitBreakStmt(stmt: Stmt.Break.type): String = "break"
  
  override def visitEmptyStmt(stmt: Stmt.Empty.type): String = ""

  override def visitFunctionExpr(expr: Expr.Function): String =
    s"""
       |(lambda
       | (${expr.params.map(_.lexeme).mkString(" ")})
       | ${expr.body.accept(this)})"""
      .stripMargin
      .replace("\n", "")

object AstPrinter:
  def main(args: Array[String]): Unit =
    val bytes = Files.readAllBytes(Paths.get("test/while/break.lox"))
    val source = new String(bytes, Charset.defaultCharset)
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens
    val parser = new Parser(tokens)
    val statements = parser.parse
    val printer = AstPrinter()
    statements.map(printer.print).foreach(println)

