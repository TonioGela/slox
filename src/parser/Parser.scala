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
      statements.addOne(declaration())
    }
    statements.toList
  }

  private def declaration(): Stmt =
    try {
      if matches(VAR) then varDeclaration() else statement()
    } catch {
      case _: ParseError => synchronize(); null
    }

  private def statement(): Stmt =
    if matches(FOR) then forStatement()
    else if matches(IF) then ifStatement()
    else if matches(PRINT) then printStatement()
    else if matches(WHILE) then whileStatement()
    else if matches(LEFT_BRACE) then Block(block())
    else expressionStatement()

  private def forStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer: Option[Stmt] =
      if matches(SEMICOLON) then None
      else if matches(VAR) then Some(varDeclaration())
      else Some(expressionStatement())

    val condition: Option[Expr] = Option.unless(check(SEMICOLON))(expression())
    consume(SEMICOLON, "Expect ';' after loop condition.")

    val increment: Option[Expr] = Option.unless(check(RIGHT_PAREN))(expression())
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    val body: Stmt = statement()

    val bodyWithIncrement: Stmt =
      increment.fold(body)(incExpr => Block(body :: Expression(incExpr) :: Nil))

    val whileLoop: Stmt = While(
      condition.getOrElse(Literal(true)),
      bodyWithIncrement,
    )

    initializer.fold(whileLoop)(initStmt => Block(initStmt :: whileLoop :: Nil))

  end forStatement

  private def whileStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition: Expr = expression()
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body: Stmt      = statement()
    While(condition, body)

  private def ifStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition: Expr  = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch: Stmt = statement()
    var elseBranch: Stmt = if matches(ELSE) then statement() else null
    If(condition, thenBranch, elseBranch)

  private def block(): List[Stmt] = {
    val statements = ListBuffer.empty[Stmt]
    while !check(RIGHT_BRACE) && !isAtEnd() do {
      statements.addOne(declaration())
    }
    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements.toList
  }

  private def varDeclaration(): Stmt =
    val name: Token       = consume(IDENTIFIER, "Expect variable name.")
    var initializer: Expr = if matches(EQUAL) then expression() else null
    consume(
      SEMICOLON,
      "Expect ';' after variable declaration.",
    )
    Var(name, initializer)

  private def printStatement(): Stmt =
    val value: Expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Print(value)

  private def expressionStatement(): Stmt =
    val value: Expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Expression(value)

  private def expression(): Expr = assignment()

  private def or(): Expr =
    var expr: Expr = and()
    while matches(OR) do {
      val operator: Token = previous()
      val right: Expr     = and()
      expr = Logical(expr, operator, right)
    }
    expr

  private def and(): Expr =
    var expr: Expr = equality()
    while matches(AND) do {
      val operator: Token = previous()
      val right: Expr     = equality()
      expr = Logical(expr, operator, right)
    }
    expr

  private def assignment(): Expr =
    val expr: Expr = or()
    if matches(EQUAL) then {
      val equals: Token = previous()
      val value: Expr   = assignment()
      expr match
        case Variable(name) => Assign(name, value)
        case _              =>
          error(equals, "Invalid assignment target.")
          expr
    } else expr

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
    if matches(FALSE) then return Literal(false)
    if matches(TRUE) then return Literal(true)
    if matches(NIL) then return Literal(null)
    if matches(NUMBER, STRING) then return Literal(previous().literal)
    if matches(IDENTIFIER) then return Variable(previous())
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
