
//> Representing Code generate-ast
package com.craftinginterpreters
package scala.tool


import java.io.{IOException, PrintWriter}
import java.nio.file.{Files, Path}
import _root_.scala.collection.immutable.List


object GenerateAst {
  def parseClassDef(str: String): ClassDef =
    val Array(className, params) = str.split(":").map(_.trim)
    ClassDef(className, params.split(", ").map(parseParamDef).toList)
    
  def parseParamDef(str: String): ParamDef =
    val Array(typeName, paramName) = str.split(" ").map(_.trim)
    ParamDef(
      fieldNameMapping(paramName),
      typeNameMapping(typeName))
    
  def fieldNameMapping(str: String): String =
    str match
      case "object" => "obj"
      case _ => str
      
  def typeNameMapping(str: String): String =
    val x = str match
      case "Object" => "Any"
      case _ => str
    x.replace('<', '[').replace('>', ']')

  case class ParamDef(name: String, typeName: String)
  case class ClassDef(name: String, params: List[ParamDef])

  @throws[IOException]
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate_ast <output directory>")
      System.exit(64)
    }
    val outputDir = args(0)
    if Files.notExists(Path.of(outputDir)) then Files.createDirectories(Path.of(outputDir))
    //> call-define-ast
    defineAst(outputDir, "Expr", List( //> Statements and State assign-expr
      "Assign   : Token name, Expr value", //< Statements and State assign-expr
      "Binary   : Expr left, Token operator, Expr right", //> Functions call-expr
      "Call     : Expr callee, Token paren, List<Expr> arguments", //< Functions call-expr
      //> Classes get-ast
      "Get      : Expr object, Token name", //< Classes get-ast
      "Grouping : Expr expression", "Literal  : Object value", //> Control Flow logical-ast
      "Logical  : Expr left, Token operator, Expr right", //< Control Flow logical-ast
      //> Classes set-ast
      "Set      : Expr object, Token name, Expr value", //< Classes set-ast
      //> Inheritance super-expr
      "Super    : Token keyword, Token method", //< Inheritance super-expr
      //> Classes this-ast
      "This     : Token keyword", //< Classes this-ast
      /* Representing Code call-define-ast < Statements and State var-expr
            "Unary    : Token operator, Expr right"
      */
      //> Statements and State var-expr
      "Unary    : Token operator, Expr right", "Variable : Token name"))
    //> Statements and State stmt-ast
    defineAst(outputDir, "Stmt", List( //> block-ast
      "Block      : List<Stmt> statements", //< block-ast
      /* Classes class-ast < Inheritance superclass-ast
            "Class      : Token name, List<Stmt.Function> methods",
      */
      //> Inheritance superclass-ast
      "Class      : Token name, Expr.Variable superclass," + " List<Stmt.Function> methods", //< Inheritance superclass-ast
      "Expression : Expr expression", //> Functions function-ast
      "Function   : Token name, List<Token> params," + " List<Stmt> body", //< Functions function-ast
      //> Control Flow if-ast
      "If         : Expr condition, Stmt thenBranch," + " Stmt elseBranch", //< Control Flow if-ast
      /* Statements and State stmt-ast < Statements and State var-stmt-ast
            "Print      : Expr expression"
      */
      //> var-stmt-ast
      "Print      : Expr expression", //< var-stmt-ast
      //> Functions return-ast
      "Return     : Token keyword, Expr value", //< Functions return-ast
      /* Statements and State var-stmt-ast < Control Flow while-ast
            "Var        : Token name, Expr initializer"
      */
      //> Control Flow while-ast
      "Var        : Token name, Expr initializer", "While      : Expr condition, Stmt body"))
    //< Statements and State stmt-ast
    //< call-define-ast
  }

  //> define-ast
  @throws[IOException]
  private def defineAst(outputDir: String, baseName: String, types: List[String]): Unit = {
    val path = s"$outputDir/$baseName.scala"
    val writer = new PrintWriter(path, "UTF-8")
    //> omit
    writer.println("//> Appendix II " + baseName.toLowerCase)
    //< omit
    writer.println(
      s"""
         |package com.craftinginterpreters
         |package scala.lox
         |
         |import _root_.scala.collection.immutable.List
         |
         |sealed abstract class $baseName:
         |  def accept[R](visitor: $baseName.Visitor[R]): R =
         |    this match""".stripMargin)
    
    val classes = types.map(parseClassDef)
    for clz <- classes do
      writer.println(s"      case x: $baseName.${clz.name} => visitor.visit${clz.name}$baseName(x)")
      
    writer.println()
    writer.println(s"object $baseName:")
    //> call-define-visitor
    defineVisitor(writer, baseName, classes)
    //< call-define-visitor
    writer.println("// Nested " + baseName + " classes here...")
    //< omit
    //> nested-classes
    // The AST classes.
    for (clz <- classes) {
      val typeName = clz.name
      val params = clz.params // [robust]

      defineType(writer, baseName, typeName, params)
    }
    //< nested-classes
    //> base-accept-method
    // The base accept() method.
    writer.println()
    //> omit
    writer.println("//< Appendix II " + baseName.toLowerCase)
    //< omit
    writer.close()
  }

  //< define-ast
  //> define-visitor
  private def defineVisitor(writer: PrintWriter, baseName: String, classes: List[ClassDef]): Unit = 
    //< omit
    writer.println("  trait Visitor[R]:")
    for (clz <- classes) {
      val typeName = clz.name
      writer.println(s"    def visit$typeName$baseName(${baseName.toLowerCase}: $baseName.$typeName): R")
    }
    writer.println()
    
  //< define-visitor
  //> define-type
  private def defineType(writer: PrintWriter, baseName: String, className: String, params: List[ParamDef]): Unit = {
    //> omit
    writer.println(s"//> ${baseName.toLowerCase}-${className.toLowerCase}")
    //< omit
    writer.print(s"  class $className")
    //> omit
    // Hack. Stmt.Class has such a long constructor that it overflows
    // the line length on the Appendix II page. Wrap it.
    // Store parameters in fields.
    // Constructor.
    var prefix = "("
    val suffix = s") extends $baseName"
    var separator = ", "
    var mapper = (param: ParamDef) => s"val ${param.name}: ${param.typeName}"
    if params.length > 2 then
      prefix = "(\n"
      mapper = (param: ParamDef) => s"    val ${param.name}: ${param.typeName}"
      separator = ",\n"

    writer.println(params
      .map(mapper)
      .mkString(prefix, separator, suffix))

    //> accept-method
    // Visitor pattern.
    //> omit
    writer.println(s"//< ${baseName.toLowerCase}-${className.toLowerCase}")
    //< omit
  }

  //< define-type
  //> pastry-visitor
  private[tool] trait PastryVisitor {
    def visitBeignet(beignet: GenerateAst#Beignet): Unit // [overload]

    def visitCruller(cruller: GenerateAst#Cruller): Unit
  }
}

class GenerateAst { //< pastry-visitor
  //> pastries
  abstract private[tool] class Pastry {
    //> pastry-accept
    private[tool] def accept(visitor: GenerateAst.PastryVisitor): Unit
    //< pastry-accept
  }

  private[tool] class Beignet extends Pastry {
    //> beignet-accept
    override private[tool] def accept(visitor: GenerateAst.PastryVisitor): Unit = {
      visitor.visitBeignet(this)
    }
    //< beignet-accept
  }

  private[tool] class Cruller extends Pastry {
    //> cruller-accept
    override private[tool] def accept(visitor: GenerateAst.PastryVisitor): Unit = {
      visitor.visitCruller(this)
    }
    //< cruller-accept
  }
  //< pastries
}

