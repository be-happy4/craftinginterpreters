
package com.craftinginterpreters.scala.lox

import com.craftinginterpreters.scala.lox.TokenType.*

import java.nio.file.Path
import scala.collection.immutable.{List, Map}
import scala.collection.mutable.ListBuffer


object Scanner:
  private val keywords: Map[String, TokenType] = TokenType.values
    .filter(_.isKeyword)
    .map(token => token.key -> token)
    .toMap

  def apply(source: String): Scanner =
    new Scanner(source)

  def apply(path: Path): Scanner =
    new Scanner(String(java.nio.file.Files.readAllBytes(path)))


class Scanner(private val source: String):
  final private val tokens = new ListBuffer[Token]
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens: List[Token] = {
    while (!isAtEnd) {
      // We are at the beginning of the next lexeme.
      start = current
      scanToken()
    }
    tokens += new Token(EOF, "", null, line)
    tokens.toList
  }

  private def scanToken(): Unit = {
    val c = advance
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '?' => addToken(QUESTION)
      case ':' => addToken(COLON)
      // [slash]

      case '!' => addToken(if (matches('=')) BANG_EQUAL
      else BANG)
      case '=' => addToken(if (matches('=')) EQUAL_EQUAL
      else EQUAL)
      case '<' => addToken(if (matches('=')) LESS_EQUAL
      else LESS)
      case '>' => addToken(if (matches('=')) GREATER_EQUAL
      else GREATER)

      case '/' =>
        if (matches('/')) {
          // A comment goes until the end of the line.
          while (peek != '\n' && !isAtEnd) advance
        } else if (matches('*')) {
          var nc = peek
          var flag = true
          while (!isAtEnd && flag) {
            nc match
              case '*' => if (matches('/')) {
                flag = false
              }
              case '\n' => line += 1
              case _ =>
            nc = advance
          }
        } else addToken(SLASH)

      case ' ' =>
      case '\r' =>
      case '\t' =>
      // Ignore whitespace.
      case '\n' =>
        line += 1

      case '"' =>
        string()

      case _ =>
        /* Scanning char-error < Scanning digit-start
                Lox.error(line, "Unexpected character.");
        */
        if (isDigit(c)) number()
        else if (isAlpha(c)) identifier()
        else Lox.error(line, "Unexpected character.")

    }
  }

  private def identifier(): Unit = {
    while (isAlphaNumeric(peek)) advance
    /* Scanning identifier < Scanning keyword-type
        addToken(IDENTIFIER);
    */
    val text = source.substring(start, current)
    var typ = Scanner.keywords.getOrElse(text, null)
    if (typ == null) typ = IDENTIFIER
    addToken(typ)
  }

  private def number(): Unit = {
    while (isDigit(peek)) advance
    // Look for a fractional part.
    if (peek == '.' && isDigit(peekNext)) {
      // Consume the "."
      advance
      while (isDigit(peek)) advance
    }
    addToken(NUMBER, source.substring(start, current).toDouble)
  }

  private def string(): Unit = {
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') line += 1
      advance
    }
    if (isAtEnd) {
      Lox.error(line, "Unterminated string.")
      return
    }
    // The closing ".
    advance
    // Trim the surrounding quotes.
    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)
  }

  private def matches(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source(current) != expected) return false
    current += 1
    true
  }

  private def peek: Char = {
    if (isAtEnd) return '\u0000'
    source(current)
  }

  private def peekNext: Char = {
    if (current + 1 >= source.length) return '\u0000'
    source(current + 1)
  } // [peek-next]

  private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  private def isDigit(c: Char) = c >= '0' && c <= '9' // [is-digit]

  private def isAtEnd = current >= source.length

  private def advance = source({
    current += 1;
    current - 1
  })

  private def addToken(typ: TokenType): Unit = {
    addToken(typ, null)
  }

  private def addToken(typ: TokenType, literal: Any): Unit = {
    val text = source.substring(start, current)
    tokens += new Token(typ, text, literal, line)
  }


