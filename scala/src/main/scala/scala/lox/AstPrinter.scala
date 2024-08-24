package com.craftinginterpreters
package scala.lox

//> Representing Code ast-printer
//> omit//> omit

//< omit//< omit

/* Representing Code ast-printer < Statements and State omit
class AstPrinter implements Expr.Visitor<String> {
*/
//> Statements and State omit
class AstPrinter extends Expr.Visitor[String] with Stmt.Visitor[String]:
  //< Statements and State omit
  private[lox] def print(expr: Expr) = expr.accept(this)

  //> Statements and State omit
  private[lox] def print(stmt: Stmt) = stmt.accept(this)

  //< Statements and State omit
  //> visit-methods
  //> Statements and State omit
  override def visitBlockStmt(stmt: Stmt.Block): String = {
    val builder = new StringBuilder
    builder.append("(block ")
    for (statement <- stmt.statements) {
      builder.append(statement.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  //< Statements and State omit
  //> Classes omit
  override def visitClassStmt(stmt: Stmt.Class): String = {
    val builder = new StringBuilder
    builder.append("(class " + stmt.name.lexeme)
    //> Inheritance omit
    if (stmt.superclass != null) builder.append(" < " + print(stmt.superclass))
    //< Inheritance omit
    for (method <- stmt.methods) {
      builder.append(" " + print(method))
    }
    builder.append(")")
    builder.toString
  }

  //< Classes omit
  //> Statements and State omit
  override def visitExpressionStmt(stmt: Stmt.Expression): String = parenthesize(";", stmt.expression)

  //< Statements and State omit
  //> Functions omit
  override def visitFunctionStmt(stmt: Stmt.Function): String = {
    val builder = new StringBuilder
    builder.append("(fun " + stmt.name.lexeme + "(")
    for (param <- stmt.params) {
      if (param ne stmt.params.head) builder.append(" ")
      builder.append(param.lexeme)
    }
    builder.append(") ")
    for (body <- stmt.body) {
      builder.append(body.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  //< Functions omit
  //> Control Flow omit
  override def visitIfStmt(stmt: Stmt.If): String = {
    if (stmt.elseBranch == null) return parenthesize2("if", stmt.condition, stmt.thenBranch)
    parenthesize2("if-else", stmt.condition, stmt.thenBranch, stmt.elseBranch)
  }

  //< Control Flow omit
  //> Statements and State omit
  override def visitPrintStmt(stmt: Stmt.Print): String = parenthesize("print", stmt.expression)

  //< Statements and State omit
  //> Functions omit
  override def visitReturnStmt(stmt: Stmt.Return): String = {
    if (stmt.value == null) return "(return)"
    parenthesize("return", stmt.value)
  }

  //< Functions omit
  //> Statements and State omit
  override def visitVarStmt(stmt: Stmt.Var): String = {
    if (stmt.initializer == null) return parenthesize2("var", stmt.name)
    parenthesize2("var", stmt.name, "=", stmt.initializer)
  }

  //< Statements and State omit
  //> Control Flow omit
  override def visitWhileStmt(stmt: Stmt.While): String = parenthesize2("while", stmt.condition, stmt.body)

  //< Control Flow omit
  //> Statements and State omit
  override def visitAssignExpr(expr: Expr.Assign): String = parenthesize2("=", expr.name.lexeme, expr.value)

  //< Statements and State omit
  override def visitBinaryExpr(expr: Expr.Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  //> Functions omit
  override def visitCallExpr(expr: Expr.Call): String = parenthesize2("call", expr.callee, expr.arguments)

  //< Functions omit
  //> Classes omit
  override def visitGetExpr(expr: Expr.Get): String = parenthesize2(".", expr.obj, expr.name.lexeme)

  //< Classes omit
  override def visitGroupingExpr(expr: Expr.Grouping): String = parenthesize("group", expr.expression)

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) return "nil"
    expr.value.toString
  }

  //> Control Flow omit
  override def visitLogicalExpr(expr: Expr.Logical): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  //< Control Flow omit
  //> Classes omit
  override def visitSetExpr(expr: Expr.Set): String = parenthesize2("=", expr.obj, expr.name.lexeme, expr.value)

  //< Classes omit
  //> Inheritance omit
  override def visitSuperExpr(expr: Expr.Super): String = parenthesize2("super", expr.method)

  //< Inheritance omit
  //> Classes omit
  override def visitThisExpr(expr: Expr.This) = "this"

  //< Classes omit
  override def visitUnaryExpr(expr: Expr.Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  //> Statements and State omit
  override def visitVariableExpr(expr: Expr.Variable): String = expr.name.lexeme

  //< Statements and State omit
  //< visit-methods
  //> print-utilities
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

  //< print-utilities
  //> omit
  // Note: AstPrinting other types of syntax trees is not shown in the
  // book, but this is provided here as a reference for those reading
  // the full code.
  private def parenthesize2(name: String, parts: AnyRef*) = {
    val builder = new StringBuilder
    builder.append("(").append(name)
    transform(builder, parts)
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

object AstPrinter:
  //< omit
  def main(args: Array[String]): Unit =
    val expression = new Expr.Binary(
      new Expr.Unary(
        new Token(TokenType.MINUS, "-", null, 1),
        new Expr.Literal(123)),
      new Token(TokenType.STAR, "*", null, 1),
      new Expr.Grouping(
        new Expr.Literal(45.67)))

    println(new AstPrinter().print(expression))

