package com.craftinginterpreters.scala

import com.craftinginterpreters.constant.Constants
import com.craftinginterpreters.scala.BaseTest.BASE_TEST_PATH
import com.craftinginterpreters.scala.lox.*

import java.nio.file.{Files, Path}
import java.util.stream.Stream
import scala.util.chaining.scalaUtilChainingOps

object BaseTest:
  private var path = Path.of(".").toAbsolutePath
  while
    !Files.exists(path.resolve("test").resolve("empty_file.lox"))
  do
    path = path.getParent
  val BASE_TEST_PATH: Path = path.resolve("test")


trait BaseTest extends munit.FunSuite:
  def absolutePath(path: String | Path): Path =
    path match
      case p: String => BASE_TEST_PATH.resolve(p)
      case p: Path => BASE_TEST_PATH.resolve(p)

  def fillExtname(path: String): String =
    if path.endsWith(Constants.extname) then path else path + Constants.extname

  def fillExtname(path: Path): Path =
    if path.endsWith(Constants.extname) then path
    else path.resolveSibling(path.getFileName.toString + Constants.extname)

  def source(first: String, more: String*): String =
    Path.of(first, more*).pipe(source)

  def source(path: Path): String =
    path
      .pipe(absolutePath)
      .pipe(fillExtname)
      .pipe(source0)

  private def source0(absolutePath: Path): String =
    absolutePath
      .pipe(Files.readAllBytes)
      .pipe(String(_))

  def sources(first: String, more: String*): Stream[String] =
    Path.of(first, more*).pipe(sources)

  def sources(path: Path): Stream[String] =
    path.pipe(absolutePath)
      .pipe(fillExtname)
      .pipe(Files.walk(_))
      .filter(_.toFile.isFile)
      .map(source0)

  def scan(source: String): List[Token] =
    Scanner(source).scanTokens

  def parse(tokens: List[Token]): List[Stmt] =
    Parser(tokens).parse