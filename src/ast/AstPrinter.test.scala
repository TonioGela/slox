package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.scanner.TokenType.*
import munit.*

class AstPrinterTest extends FunSuite:

  test("Ast printer should produce matching expressions") {
    val expression: Expr = Binary(
      Unary(Token(MINUS, "-", null, 1), Literal(123)),
      Token(STAR, "*", null, 1),
      Grouping(Literal(45.67)),
    )

    assertEquals(
      AstPrinter.print(expression),
      "(* (- 123) (group 45.67))",
    )
  }
