package com.craftinginterpreters.scala.lox



import com.craftinginterpreters.scala.lox.TokenType.*

import java.nio.file.Path
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer


object Parser:
  private class ParseError extends RuntimeException {}

  def main(args: Array[String]): Unit =
    val path = Path.of("test/if/if.lox")
    val source = String(java.nio.file.Files.readAllBytes(path))
    val statements = Parser(Scanner(source).scanTokens).parse
    val printer = AstPrinter()
    statements.map(printer.print).foreach(println)


class Parser(
  private val tokens: List[Token]) {
  private var current = 0
  private var loopDepth = 0

  /* Parsing Expressions parse < Statements and State parse
    Expr parse() {
      try {
        return expression();
      } catch (ParseError error) {
        return null;
      }
    }
  */
  def parse: List[Stmt] =
    val statements = new ListBuffer[Stmt]
    while (!isAtEnd) {
      /* Statements and State parse < Statements and State parse-declaration
            statements.add(statement());
      */
      statements += declaration
    }
    statements.toList // [parse-error-handling]


  private def expression: Expr =
    /* Parsing Expressions expression < Statements and State expression
        return equality();
    */
    assignment

  private def declaration: Stmt = try {
    if (matches(CLASS)) return classDeclaration
    if (check(FUN) && checkNext(IDENTIFIER)) {
      consume(FUN, null)
      return function("function")
    }
    if (matches(VAR)) return varDeclaration
    statement
  } catch {
    case _: Parser.ParseError =>
      synchronize()
      null
  }

  private def classDeclaration: Stmt.Class =
    val name = consume(IDENTIFIER, "Expect class name.")
    var superclass: Expr.Variable = null
    if (matches(LESS)) {
      consume(IDENTIFIER, "Expect superclass name.")
      superclass = Expr.Variable(previous)
    }
    consume(LEFT_BRACE, "Expect '{' before class body.")
    val methods = new ListBuffer[Stmt.Function]
    while (matches(FUN))
      methods += function("method")
    consume(RIGHT_BRACE, "Expect '}' after class body.")
    /* Classes parse-class-declaration < Inheritance construct-class-ast
        return new Stmt.Class(name, methods);
    */
    Stmt.Class(name, superclass, methods.toList)

  private def statement: Stmt =
    if (matches(FOR)) return forStatement
    if (matches(IF)) return ifStatement
    if (matches(PRINT)) return printStatement
    if (matches(RETURN)) return returnStatement
    if (matches(WHILE)) return whileStatement
    if (matches(LEFT_BRACE)) return block
    if (matches(BREAK)) return breakStatement
    if (matches(SEMICOLON)) return Stmt.Empty
    expressionStatement

  private def forStatement: Stmt = try {
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    /* Control Flow for-statement < Control Flow for-initializer
        // More here...
    */
    var initializer: Stmt = null
    if (matches(SEMICOLON)) initializer = null
    else if (matches(VAR)) initializer = varDeclaration
    else initializer = expressionStatement
    var condition: Expr = null
    if (!check(SEMICOLON)) condition = expression
    consume(SEMICOLON, "Expect ';' after loop condition.")
    var increment: Expr = null
    if (!check(RIGHT_PAREN)) increment = expression
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")
    var body = statement
    if (increment != null) body = Stmt.Block(List(body, increment))
    if (condition == null) condition = Expr.Literal(true)
    body = Stmt.While(condition, body)
    if (initializer != null) body = Stmt.Block(List(initializer, body))
    body
  } finally loopDepth -= 1

  private def ifStatement: Stmt.If =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after if condition.") // [parens]

    val thenBranch = statement
    var elseBranch: Stmt = null
    if (matches(ELSE)) elseBranch = statement
    Stmt.If(condition, thenBranch, elseBranch)

  private def printStatement: Stmt.Print =
    val value = expression
    consume(SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)

  private def returnStatement: Stmt.Return =
    val keyword = previous
    var value: Expr = null
    if (!check(SEMICOLON)) value = expression
    consume(SEMICOLON, "Expect ';' after return value.")
    Stmt.Return(keyword, value)

  private def varDeclaration: Stmt.Var =
    val name = consume(IDENTIFIER, "Expect variable name.")
    var initializer: Expr = null
    if (matches(EQUAL)) initializer = expression
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)

  private def whileStatement: Stmt.While = try {
    loopDepth += 1
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement
    Stmt.While(condition, body)
  } finally loopDepth -= 1

  private def expressionStatement: Expr =
    val expr = expression
    consume(SEMICOLON, "Expect ';' after expression.")
    expr

  private def function(kind: String): Stmt.Function =
    val name = consume(IDENTIFIER, "Expect " + kind + " name.")
    Stmt.Function(name, functionBody(kind))

  def functionBody(kind: String): Expr.Function =
    consume(LEFT_PAREN, "Expect '(' after " + kind + " name.")
    val params = new ListBuffer[Token]
    if (!check(RIGHT_PAREN)) then
      while
        if (params.size >= 255) error(peek, "Can't have more than 255 parameters.")
        params += consume(IDENTIFIER, "Expect parameter name.")
        matches(COMMA)
      do ()
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")
    Expr.Function(params.toList, block)

  private def block: Stmt.Block =
    val statements = new ListBuffer[Stmt]
    while (!check(RIGHT_BRACE) && !isAtEnd) statements += declaration
    consume(RIGHT_BRACE, "Expect '}' after block.")
    Stmt.Block(statements.toList)

  private def breakStatement: Stmt.Break.type =
    if loopDepth == 0 then
      error(previous, "Must be inside a loop to use 'break'.")
    consume(SEMICOLON, "Expect ';' after 'break'.")
    Stmt.Break

  private def assignment: Expr =
    /* Statements and State parse-assignment < Control Flow or-in-assignment
        Expr expr = equality();
    */
    var expr = ternary
    if (matches(EQUAL)) {
      val equals = previous
      val value = assignment
      expr = expr match
        case variable: Expr.Variable =>
          Expr.Assign(variable.name, value)
        case get: Expr.Get =>
          Expr.Set(get.obj, get.name, value)
        case _ =>
          error(equals, "Invalid assignment target.") // [no-throw]
          expr
    }
    expr

  private def ternary: Expr =
    val expr = or
    if (matches(QUESTION)) {
      val positiveExpression = ternary
      consume(COLON, "Expect ':' after '?' expression.")
      Expr.Ternary(expr, positiveExpression, ternary)
    } else expr

  private def or: Expr =
    var expr = and
    while (matches(OR)) {
      val operator = previous
      val right = and
      expr = Expr.Logical(expr, operator, right)
    }
    expr

  private def and: Expr =
    var expr = equality
    while (matches(AND)) {
      val operator = previous
      val right = equality
      expr = Expr.Logical(expr, operator, right)
    }
    expr

  private def equality: Expr =
    var expr = comparison
    while (matches(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous
      val right = comparison
      expr = Expr.Binary(expr, operator, right)
    }
    expr

  private def comparison: Expr =
    var expr = term
    while (matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right = term
      expr = Expr.Binary(expr, operator, right)
    }
    expr

  private def term: Expr =
    var expr = factor
    while (matches(MINUS, PLUS)) {
      val operator = previous
      val right = factor
      expr = Expr.Binary(expr, operator, right)
    }
    expr

  private def factor: Expr =
    var expr = unary
    while (matches(SLASH, STAR)) {
      val operator = previous
      val right = unary
      expr = Expr.Binary(expr, operator, right)
    }
    expr

  private def unary: Expr =
    if (matches(BANG, MINUS)) {
      val operator = previous
      val right = unary
      return Expr.Unary(operator, right)
    }
    /* Parsing Expressions unary < Functions unary-call
        return primary();
    */
    call

  private def finishCall(callee: Expr): Expr.Call =
    val arguments = new ListBuffer[Expr]
    if !check(RIGHT_PAREN) then
      while
        if (arguments.size >= 255) error(peek, "Can't have more than 255 arguments.")
        arguments += expression
        matches(COMMA)
      do ()
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    Expr.Call(callee, paren, arguments.toList)

  private def call: Expr =
    var expr = primary
    var flag = true
    while flag do // [while-true]
      if (matches(LEFT_PAREN)) expr = finishCall(expr)
      else if (matches(DOT)) {
        val name = consume(IDENTIFIER, "Expect property name after '.'.")
        expr = Expr.Get(expr, name)
      } else flag = false
    expr

  private def primary: Expr =
    if (matches(FALSE)) return Expr.Literal(false)
    if (matches(TRUE)) return Expr.Literal(true)
    if (matches(NIL)) return Expr.Literal(null)
    if (matches(NUMBER, STRING)) return Expr.Literal(previous.literal)
    if (matches(SUPER)) {
      val keyword = previous
      consume(DOT, "Expect '.' after 'super'.")
      val method = consume(IDENTIFIER, "Expect superclass method name.")
      return Expr.Super(keyword, method)
    }
    if (matches(THIS)) return Expr.This(previous)
    if (matches(IDENTIFIER)) return Expr.Variable(previous)
    if (matches(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return Expr.Grouping(expr)
    }
    if (matches(FUN)) return functionBody("function")
    throw error(peek, "Expect expression.")

  private def matches(types: TokenType*): Boolean =
    var flag = true
    for (typ <- types if flag) {
      if (check(typ)) {
        advance
        flag = false
      }
    }
    !flag

  private def consume(typ: TokenType, message: String): Token =
    if (check(typ)) return advance
    throw error(peek, message)

  private def check(typ: TokenType): Boolean =
    if (isAtEnd) return false
    peek.typ eq typ

  private def checkNext(tokenType: TokenType): Boolean =
    if (isAtEnd) return false
    if (tokens(current + 1).typ == EOF) return false
    tokens(current + 1).typ == tokenType
    
  private def advance =
    if (!isAtEnd) current += 1
    previous

  private def isAtEnd: Boolean = peek.typ eq EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  private def error(token: Token, message: String): RuntimeException =
    Lox.error(token, message)
    new Parser.ParseError

  private def synchronize(): Unit =
    advance
    while (!isAtEnd) {
      if (previous.typ eq SEMICOLON) return
      peek.typ match {
        case CLASS =>
        case FUN =>
        case VAR =>
        case FOR =>
        case IF =>
        case WHILE =>
        case PRINT =>
        case RETURN =>
          return
        case _ =>
      }
      advance
    }
}
