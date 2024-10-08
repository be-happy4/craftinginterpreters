package com.craftinginterpreters.scala.lox

//> Parsing Expressions parser

//> Statements and State parser-imports//> Statements and State parser-imports

import com.craftinginterpreters.scala.lox.TokenType.*

import java.nio.file.Path
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer


object Parser:
  //> parse-error
  private class ParseError extends RuntimeException {}

  def main(args: Array[String]): Unit =
    val path = Path.of("test/if/if.lox")
    val source = String(java.nio.file.Files.readAllBytes(path))
    val statements = Parser(Scanner(source).scanTokens).parse
    val printer = AstPrinter()
    statements.map(printer.print).foreach(println)


class Parser( //< parse-error
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
  //> Statements and State parse
  def parse: List[Stmt] =
    val statements = new ListBuffer[Stmt]
    while (!isAtEnd) {
      /* Statements and State parse < Statements and State parse-declaration
            statements.add(statement());
      */
      //> parse-declaration
      statements += declaration
    }
    statements.toList // [parse-error-handling]


  //< Statements and State parse
  //> expression
  private def expression: Expr =
    /* Parsing Expressions expression < Statements and State expression
        return equality();
    */
    //> Statements and State expression
    assignment
  //< Statements and State expression

//  private def comma: Expr =
//    val expr = assignment
//    if (matches(COMMA))
//      Expr.Comma(expr, comma)
//    else expr
//  //< Statements and State expression

  //< expression
  //> Statements and State declaration
  private def declaration: Stmt = try {
    //> Classes match-class
    if (matches(CLASS)) return classDeclaration
    //< Classes match-class
    //> Functions match-fun
    if (check(FUN) && checkNext(IDENTIFIER)) {
      consume(FUN, null)
      return function("function")
    }
    //< Functions match-fun
    if (matches(VAR)) return varDeclaration
    statement
  } catch {
    case error: Parser.ParseError =>
      synchronize()
      null
  }

  //< Statements and State declaration
  //> Classes parse-class-declaration
  private def classDeclaration: Stmt.Class =
    val name = consume(IDENTIFIER, "Expect class name.")
    //> Inheritance parse-superclass
    var superclass: Expr.Variable = null
    if (matches(LESS)) {
      consume(IDENTIFIER, "Expect superclass name.")
      superclass = new Expr.Variable(previous)
    }
    //< Inheritance parse-superclass
    consume(LEFT_BRACE, "Expect '{' before class body.")
    val methods = new ListBuffer[Stmt.Function]
    while (!check(RIGHT_BRACE) && !isAtEnd) methods += function("method")
    consume(RIGHT_BRACE, "Expect '}' after class body.")
    /* Classes parse-class-declaration < Inheritance construct-class-ast
        return new Stmt.Class(name, methods);
    */
    //> Inheritance construct-class-ast
    new Stmt.Class(name, superclass, methods.toList)
  //< Inheritance construct-class-ast

  //< Classes parse-class-declaration
  //> Statements and State parse-statement
  private def statement: Stmt =
    //> Control Flow match-for
    if (matches(FOR)) return forStatement
    //< Control Flow match-for
    //> Control Flow match-if
    if (matches(IF)) return ifStatement
    //< Control Flow match-if
    if (matches(PRINT)) return printStatement
    //> Functions match-return
    if (matches(RETURN)) return returnStatement
    //< Functions match-return
    //> Control Flow match-while
    if (matches(WHILE)) return whileStatement
    //< Control Flow match-while
    //> parse-block
    if (matches(LEFT_BRACE)) return block
    //< parse-block
    if (matches(BREAK)) return breakStatement
    expressionStatement

  //< Statements and State parse-statement
  //> Control Flow for-statement
  private def forStatement: Stmt = try {
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    /* Control Flow for-statement < Control Flow for-initializer
        // More here...
    */
    //> for-initializer
    var initializer: Stmt = null
    if (matches(SEMICOLON)) initializer = null
    else if (matches(VAR)) initializer = varDeclaration
    else initializer = expressionStatement
    //< for-initializer
    //> for-condition
    var condition: Expr = null
    if (!check(SEMICOLON)) condition = expression
    consume(SEMICOLON, "Expect ';' after loop condition.")
    //< for-condition
    //> for-increment
    var increment: Expr = null
    if (!check(RIGHT_PAREN)) increment = expression
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")
    //< for-increment
    //> for-body
    var body = statement
    //> for-desugar-increment
    if (increment != null) body = new Stmt.Block(List(body, new Stmt.Expression(increment)))
    //< for-desugar-increment
    //> for-desugar-condition
    if (condition == null) condition = new Expr.Literal(true)
    body = new Stmt.While(condition, body)
    //< for-desugar-condition
    //> for-desugar-initializer
    if (initializer != null) body = new Stmt.Block(List(initializer, body))
    //< for-desugar-initializer
    body
    //< for-body
  } finally loopDepth -= 1

  //< Control Flow for-statement
  //> Control Flow if-statement
  private def ifStatement: Stmt.If =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after if condition.") // [parens]

    val thenBranch = statement
    var elseBranch: Stmt = null
    if (matches(ELSE)) elseBranch = statement
    new Stmt.If(condition, thenBranch, elseBranch)

  //< Control Flow if-statement
  //> Statements and State parse-print-statement
  private def printStatement: Stmt.Print =
    val value = expression
    consume(SEMICOLON, "Expect ';' after value.")
    new Stmt.Print(value)

  //< Statements and State parse-print-statement
  //> Functions parse-return-statement
  private def returnStatement: Stmt.Return =
    val keyword = previous
    var value: Expr = null
    if (!check(SEMICOLON)) value = expression
    consume(SEMICOLON, "Expect ';' after return value.")
    new Stmt.Return(keyword, value)

  //< Functions parse-return-statement
  //> Statements and State parse-var-declaration
  private def varDeclaration: Stmt.Var =
    val name = consume(IDENTIFIER, "Expect variable name.")
    var initializer: Expr = null
    if (matches(EQUAL)) initializer = expression
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    new Stmt.Var(name, initializer)

  //< Statements and State parse-var-declaration
  //> Control Flow while-statement
  private def whileStatement: Stmt.While = try {
    loopDepth += 1
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement
    new Stmt.While(condition, body)
  } finally loopDepth -= 1

  //< Control Flow while-statement
  //> Statements and State parse-expression-statement
  private def expressionStatement: Stmt.Expression =
    val expr = expression
    consume(SEMICOLON, "Expect ';' after expression.")
    new Stmt.Expression(expr)

  //< Statements and State parse-expression-statement
  //> Functions parse-function
  private def function(kind: String): Stmt.Function =
    val name = consume(IDENTIFIER, "Expect " + kind + " name.")
    new Stmt.Function(name, functionBody(kind))
  //< parse-body

  def functionBody(kind: String): Expr.Function =
    //> parse-parameters
    consume(LEFT_PAREN, "Expect '(' after " + kind + " name.")
    val params = new ListBuffer[Token]
    if (!check(RIGHT_PAREN)) then
      while
        if (params.size >= 255) error(peek, "Can't have more than 255 parameters.")
        params += consume(IDENTIFIER, "Expect parameter name.")
        matches(COMMA)
      do ()
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    //< parse-parameters
    //> parse-body
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")
    Expr.Function(params.toList, block)

  //< Functions parse-function
  //> Statements and State block
  private def block: Stmt.Block =
    val statements = new ListBuffer[Stmt]
    while (!check(RIGHT_BRACE) && !isAtEnd) statements += declaration
    consume(RIGHT_BRACE, "Expect '}' after block.")
    Stmt.Block(statements.toList)

  private def breakStatement: Stmt.Break =
    if loopDepth == 0 then
      error(previous, "Must be inside a loop to use 'break'.");
    consume(SEMICOLON, "Expect ';' after 'break'.");
    Stmt.Break();

  //< Statements and State block
  //> Statements and State parse-assignment
  private def assignment: Expr =
    /* Statements and State parse-assignment < Control Flow or-in-assignment
        Expr expr = equality();
    */
    //> Control Flow or-in-assignment
    var expr = ternary
    //< Control Flow or-in-assignment
    if (matches(EQUAL)) {
      val equals = previous
      val value = assignment
      expr = expr match
        case variable: Expr.Variable =>
          new Expr.Assign(variable.name, value)
        case get: Expr.Get =>
          new Expr.Set(get.obj, get.name, value)
        case _ =>
          error(equals, "Invalid assignment target.") // [no-throw]
          expr
    }
    expr

  //< Statements and State parse-assignment
  //> Control Flow or
  private def ternary: Expr =
    val expr = or
    if (matches(QUESTION)) {
      val positiveExpression = ternary
      consume(COLON, "Expect ':' after '?' expression.")
      new Expr.Ternary(expr, positiveExpression, ternary)
    } else expr

  //< Statements and State parse-assignment
  //> Control Flow or
  private def or: Expr =
    var expr = and
    while (matches(OR)) {
      val operator = previous
      val right = and
      expr = new Expr.Logical(expr, operator, right)
    }
    expr

  //< Control Flow or
  //> Control Flow and
  private def and: Expr =
    var expr = equality
    while (matches(AND)) {
      val operator = previous
      val right = equality
      expr = new Expr.Logical(expr, operator, right)
    }
    expr

  //< Control Flow and
  //> equality
  private def equality: Expr =
    var expr = comparison
    while (matches(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous
      val right = comparison
      expr = new Expr.Binary(expr, operator, right)
    }
    expr

  //< equality
  //> comparison
  private def comparison: Expr =
    var expr = term
    while (matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right = term
      expr = new Expr.Binary(expr, operator, right)
    }
    expr

  //< comparison
  //> term
  private def term: Expr =
    var expr = factor
    while (matches(MINUS, PLUS)) {
      val operator = previous
      val right = factor
      expr = new Expr.Binary(expr, operator, right)
    }
    expr

  //< term
  //> factor
  private def factor: Expr =
    var expr = unary
    while (matches(SLASH, STAR)) {
      val operator = previous
      val right = unary
      expr = new Expr.Binary(expr, operator, right)
    }
    expr

  //< factor
  //> unary
  private def unary: Expr =
    if (matches(BANG, MINUS)) {
      val operator = previous
      val right = unary
      return new Expr.Unary(operator, right)
    }
    /* Parsing Expressions unary < Functions unary-call
        return primary();
    */
    //> Functions unary-call
    call
  //< Functions unary-call

  //< unary
  //> Functions finish-call
  private def finishCall(callee: Expr): Expr.Call =
    val arguments = new ListBuffer[Expr]
    if !check(RIGHT_PAREN) then
      while
        //> check-max-arity
        if (arguments.size >= 255) error(peek, "Can't have more than 255 arguments.")
        //< check-max-arity
        arguments += expression
        matches(COMMA)
      do ()
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    new Expr.Call(callee, paren, arguments.toList)

  //< Functions finish-call
  //> Functions call
  private def call: Expr =
    var expr = primary
    var flag = true
    while flag do // [while-true]
      if (matches(LEFT_PAREN)) expr = finishCall(expr)
      else if (matches(DOT)) {
        val name = consume(IDENTIFIER, "Expect property name after '.'.")
        expr = new Expr.Get(expr, name)
      } else flag = false
    expr

  //< Functions call
  //> primary
  private def primary: Expr =
    if (matches(FALSE)) return new Expr.Literal(false)
    if (matches(TRUE)) return new Expr.Literal(true)
    if (matches(NIL)) return new Expr.Literal(null)
    if (matches(NUMBER, STRING)) return new Expr.Literal(previous.literal)
    //> Inheritance parse-super
    if (matches(SUPER)) {
      val keyword = previous
      consume(DOT, "Expect '.' after 'super'.")
      val method = consume(IDENTIFIER, "Expect superclass method name.")
      return new Expr.Super(keyword, method)
    }
    //< Inheritance parse-super
    //> Classes parse-this
    if (matches(THIS)) return new Expr.This(previous)
    //< Classes parse-this
    //> Statements and State parse-identifier
    if (matches(IDENTIFIER)) return new Expr.Variable(previous)
    //< Statements and State parse-identifier
    if (matches(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return new Expr.Grouping(expr)
    }
    if (matches(FUN)) return functionBody("function")
    //> primary-error
    throw error(peek, "Expect expression.")
  //< primary-error

  //< primary
  //> match
  private def matches(types: TokenType*): Boolean =
    var flag = true
    for (typ <- types if flag) {
      if (check(typ)) {
        advance
        flag = false
      }
    }
    !flag

  //< match
  //> consume
  private def consume(typ: TokenType, message: String): Token =
    if (check(typ)) return advance
    throw error(peek, message)

  //< consume
  //> check
  private def check(typ: TokenType): Boolean =
    if (isAtEnd) return false
    peek.typ eq typ

  private def checkNext(tokenType: TokenType): Boolean =
    if (isAtEnd) return false
    if (tokens(current + 1).typ == EOF) return false
    tokens(current + 1).typ == tokenType
    
  //< check
  //> advance
  private def advance =
    if (!isAtEnd) current += 1
    previous

  //< advance
  //> utils
  private def isAtEnd: Boolean = peek.typ eq EOF

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

  //< utils
  //> error
  private def error(token: Token, message: String): RuntimeException =
    Lox.error(token, message)
    new Parser.ParseError

  //< error
  //> synchronize
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
  //< synchronize
}
