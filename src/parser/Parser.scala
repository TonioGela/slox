package dev.toniogela.lox.parser

import scala.collection.mutable.ListBuffer
import dev.toniogela.lox.Slox
import dev.toniogela.lox.ast.*
import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.scanner.TokenType.*

class Parser(val tokens: List[Token]):
  // ! TODO Mutability
  var current: Int = 0

  def parse(): List[Stmt] = {
    val statements = ListBuffer.empty[Stmt]
    while !isAtEnd() do {
      statements.addOne(statement())
    }
    statements.toList
  }

  private def statement(): Stmt =
    if matches(PRINT) then printStatement()
    else expressionStatement()

  private def printStatement(): Stmt =
    val value: Expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(value)

  private def expressionStatement(): Stmt =
    val value: Expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(value)

  private def expression(): Expr = equality()

  // * The precedence is dictated by the order
  // * in which the call chain is implemented

  // ! TODO Parsing in this shape is what you
  // ! do in case of left associative expressions
  private def equality(): Expr = {
    var expr: Expr = comparison()

    while matches(BANG_EQUAL, EQUAL_EQUAL) do {
      val operator: Token = previous()
      val right: Expr     = comparison()
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private def comparison(): Expr = {
    var expr: Expr = term()
    while matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do {
      val operator: Token = previous()
      val right: Expr     = term()
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private def term(): Expr = {
    var expr: Expr = factor()
    while matches(MINUS, PLUS) do {
      val operator: Token = previous()
      val right: Expr     = factor()
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private def factor(): Expr = {
    var expr: Expr = unary()
    while matches(SLASH, STAR) do {
      val operator: Token = previous()
      val right: Expr     = unary()
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private def unary(): Expr =
    if matches(BANG, MINUS) then {
      val operator: Token = previous()
      val right: Expr     = unary()
      new Unary(operator, right)
    } else primary()

    // ! TODO Multiple returns
  private def primary(): Expr = {
    if matches(FALSE) then return new Literal(false)
    if matches(TRUE) then return new Literal(true)
    if matches(NIL) then return new Literal(null)
    if matches(NUMBER, STRING) then return new Literal(previous().literal)
    if matches(LEFT_PAREN) then
      val expr: Expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return new Grouping(expr)
    else throw error(peek(), "Expect expression.")
  }

  private def consume(`type`: TokenType, message: String): Token =
    if check(`type`) then advance() else throw error(peek(), message)

  private def error(token: Token, message: String): ParseError =
    Slox.error(token, message)
    new ParseError()

  private def matches(types: TokenType*): Boolean = types.find(check).fold(false)(_ =>
    advance()
    true
  )

  private def check(`type`: TokenType): Boolean = !isAtEnd() && peek().`type` == `type`

  private def advance(): Token = {
    if !isAtEnd() then current = current + 1
    previous()
  }

  private def isAtEnd(): Boolean = peek().`type` == TokenType.EOF

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def synchronize(): Unit = {
    advance()
    while !isAtEnd() do {
      if previous().`type` == SEMICOLON then return ()
      else
        peek().`type` match {
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return ()
          case _                                                     => ()
        }
      advance()
    }
  }

  class ParseError extends RuntimeException

end Parser
