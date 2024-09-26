
//> Scanning lox-class
package com.craftinginterpreters.scala.lox

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}


object Lox:
  //> Evaluating Expressions interpreter-instance
  private val interpreter = new Interpreter
  //< Evaluating Expressions interpreter-instance
  //> had-error
  var hadError = false
  //< had-error
  //> Evaluating Expressions had-runtime-error-field
  var hadRuntimeError = false

  //< Evaluating Expressions had-runtime-error-field
  @throws[IOException]
  def main(args: Array[String]): Unit =
    if (args.length > 1) {
      println("Usage: jlox [script]")
      System.exit(64) // [64]
    }
    else if (args.length == 1) runFile(args(0))
    else runPrompt()

  //> run-file
  @throws[IOException]
  private def runFile(path: String): Unit =
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset))
    //> exit-code
    // Indicate an error in the exit code.
    if (hadError) System.exit(65)
    //< exit-code
    //> Evaluating Expressions check-runtime-error
    if (hadRuntimeError) System.exit(70)
  //< Evaluating Expressions check-runtime-error

  //< run-file
  //> prompt
  @throws[IOException]
  private def runPrompt(): Unit =
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    while
      // [repl]
      print("> ")
      val line = reader.readLine
      if line != null then
        run(line) match
          case results: List[_] => results.foreach {
            case () =>
            case v => println(Interpreter.stringify(v))
          }
          case _ =>
        true
      else false
    do
      //> reset-had-error
      hadError = false

  //< prompt
  //> run
  private def run(source: String): Any =
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens
    /* Scanning run < Parsing Expressions print-ast

        // For now, just print the tokens.
        for (Token token : tokens) {
          println(token);
        }
    */
    //> Parsing Expressions print-ast
    val parser = new Parser(tokens)
    /* Parsing Expressions print-ast < Statements and State parse-statements
        Expr expression = parser.parse();
    */
    //> Statements and State parse-statements
    val statements = parser.parse
    //< Statements and State parse-statements
    // Stop if there was a syntax error.
    if (hadError) return ()
    //< Parsing Expressions print-ast
    //> Resolving and Binding create-resolver
    val resolver = new Resolver(interpreter)
    resolver.resolve(statements)
    //> resolution-error
    // Stop if there was a resolution error.
    if (hadError) return ()
    //< resolution-error
    //< Resolving and Binding create-resolver
    /* Parsing Expressions print-ast < Evaluating Expressions interpreter-interpret
       println(new AstPrinter().print(expression));
    */
    /* Evaluating Expressions interpreter-interpret < Statements and State interpret-statements
        interpreter.interpret(expression);
    */
    //> Statements and State interpret-statements
    interpreter.interpret(statements)
  //< Statements and State interpret-statements

  //< run
  //> lox-error
  def error(line: Int, message: String): Unit =
    report(line, "", message)

  private def report(line: Int, where: String, message: String): Unit =
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true

  //< lox-error
  //> Parsing Expressions token-error
  def error(token: Token, message: String): Unit =
    if (token.typ eq TokenType.EOF) report(token.line, " at end", message)
    else report(token.line, " at '" + token.lexeme + "'", message)

  //< Parsing Expressions token-error
  //> Evaluating Expressions runtime-error-method
  def runtimeError(error: RuntimeError): Unit =
    System.err.println(error.getMessage + "\n[line " + error.token.line + "]")
    hadRuntimeError = true
//< Evaluating Expressions runtime-error-method

