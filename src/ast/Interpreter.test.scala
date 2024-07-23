package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.scanner.TokenType.*
import munit.*
import java.io.*

class InterpreterTest extends FunSuite:

  def testWithStdOut(testOptions: TestOptions)(f: ByteArrayOutputStream => Unit)(using
    Location
  ): Unit = test(testOptions) {
    val old: PrintStream            = System.out
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
    System.setOut(new PrintStream(baos))
    f(baos)
    System.setOut(old)
  }

  testWithStdOut("Intepreter should correctly interpret expressions") { baos =>
    val statement: Stmt = Print(Binary(
      Unary(Token(MINUS, "-", null, 1), new Literal(123d)),
      Token(STAR, "*", null, 1),
      Grouping(Literal(45.67)),
    ))

    Interpreter.interpret(statement :: Nil)
    assertEquals("-5617.41", baos.toString().trim())
  }

end InterpreterTest
