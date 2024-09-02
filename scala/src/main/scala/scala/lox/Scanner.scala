
//> Scanning scanner-class
package com.craftinginterpreters
package scala.lox

import scala.lox.TokenType.*

import java.util
import java.util.{ArrayList, HashMap, Map}
import _root_.scala.collection.mutable.ListBuffer
import _root_.scala.collection.immutable.List


object Scanner:
  //> keyword-map
  private val keywords: util.Map[String, TokenType] = new util.HashMap[String, TokenType]
  keywords.put("and", AND)
  keywords.put("class", CLASS)
  keywords.put("else", ELSE)
  keywords.put("false", FALSE)
  keywords.put("for", FOR)
  keywords.put("fun", FUN)
  keywords.put("if", IF)
  keywords.put("nil", NIL)
  keywords.put("or", OR)
  keywords.put("print", PRINT)
  keywords.put("return", RETURN)
  keywords.put("super", SUPER)
  keywords.put("this", THIS)
  keywords.put("true", TRUE)
  keywords.put("var", VAR)
  keywords.put("while", WHILE)


class Scanner private[lox](private val source: String) //< scan-state
{
  final private val tokens = new ListBuffer[Token]
  //> scan-state
  private var start = 0
  private var current = 0
  private var line = 1

  //> scan-tokens
  private[lox] def scanTokens: List[Token] = {
    while (!isAtEnd) {
      // We are at the beginning of the next lexeme.
      start = current
      scanToken()
    }
    tokens += new Token(EOF, "", null, line)
    tokens.toList
  }

  //< scan-tokens
  //> scan-token
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
      // [slash]

      //> two-char-tokens
      case '!' => addToken(if (matches('=')) BANG_EQUAL
        else BANG)
      case '=' => addToken(if (matches('=')) EQUAL_EQUAL
        else EQUAL)
      case '<' => addToken(if (matches('=')) LESS_EQUAL
        else LESS)
      case '>' => addToken(if (matches('=')) GREATER_EQUAL
        else GREATER)

      //< two-char-tokens
      //> slash
      case '/' =>
        if (matches('/')) {
          // A comment goes until the end of the line.
          while (peek != '\n' && !isAtEnd) advance
        }
        else addToken(SLASH)

      //< slash
      //> whitespace
      case ' ' =>
      case '\r' =>
      case '\t' =>
      // Ignore whitespace.
      case '\n' =>
        line += 1

      //< whitespace
      //> string-start
      case '"' =>
        string()

      //< string-start
      //> char-error
      case _ =>
        /* Scanning char-error < Scanning digit-start
                Lox.error(line, "Unexpected character.");
        */
        //> digit-start
        if (isDigit(c)) number()
        else if (isAlpha(c)) identifier()
        else Lox.error(line, "Unexpected character.")
      //< digit-start

      //< char-error
    }
  }

  //< scan-token
  //> identifier
  private def identifier(): Unit = {
    while (isAlphaNumeric(peek)) advance
    /* Scanning identifier < Scanning keyword-type
        addToken(IDENTIFIER);
    */
    //> keyword-type
    val text = source.substring(start, current)
    var typ = Scanner.keywords.get(text)
    if (typ == null) typ = IDENTIFIER
    addToken(typ)
    //< keyword-type
  }

  //< identifier
  //> number
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

  //< number
  //> string
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

  //< string
  //> match
  private def matches(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source.charAt(current) != expected) return false
    current += 1
    true
  }

  //< match
  //> peek
  private def peek: Char = {
    if (isAtEnd) return '\u0000'
    source.charAt(current)
  }

  //< peek
  //> peek-next
  private def peekNext: Char = {
    if (current + 1 >= source.length) return '\u0000'
    source.charAt(current + 1)
  } // [peek-next]

  //< peek-next
  //> is-alpha
  private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  //< is-alpha
  //> is-digit
  private def isDigit(c: Char) = c >= '0' && c <= '9' // [is-digit]

  //< is-digit
  //> is-at-end
  private def isAtEnd = current >= source.length

  //< is-at-end
  //> advance-and-add-token
  private def advance = source.charAt({
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
  //< advance-and-add-token
}

