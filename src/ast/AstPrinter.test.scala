package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.scanner.TokenType.*
import munit.*
import munit.diff.Printer

class AstPrinterTest extends FunSuite:
  override def printer: Printer = Printer(200)(PartialFunction.empty)

  test("Ast printer should produce matching expressions") {
    val expression: Expr = new Binary(
      new Unary(new Token(MINUS, "-", null, 1), new Literal(123)),
      new Token(STAR, "*", null, 1),
      new Grouping(new Literal(45.67)),
    )

    assertEquals(
      AstPrinter.print(expression),
      "(* (- 123) (group 45.67))",
    )
  }

end AstPrinterTest
