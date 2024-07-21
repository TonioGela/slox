package dev.toniogela.lox

import java.io.{BufferedReader, InputStreamReader}
import scala.io.Source
import dev.toniogela.lox.ast.*
import dev.toniogela.lox.parser.*
import dev.toniogela.lox.scanner.*
import scala.util.boundary
import dev.toniogela.lox.ast.Interpreter.RuntimeError

object Slox:
  // ! TODO Mutability
  var hadError: Boolean        = false
  var hadRuntimeError: Boolean = false

  def main(args: Array[String]): Unit = args.toList match
    case Nil         => runPrompt()
    case file :: Nil => runFile(file)
    case _           =>
      System.err.println("Usage: slox [script]")
      System.exit(64)

  def runFile(file: String): Unit =
    run(Source.fromFile(file).getLines().mkString("\n"))
    if hadError then System.exit(65)
    if hadRuntimeError then System.exit(70)

  def runPrompt(): Unit =
    def readEval(reader: BufferedReader): Unit = boundary {
      print("> ")
      Option(reader.readLine()).filter(_.nonEmpty).fold {
        println("Bye bye!")
        boundary.break()
      }(run)
      hadError = false
      hadRuntimeError = false
      readEval(reader)
    }
    readEval(new BufferedReader(new InputStreamReader(System.in)))

  def run(source: String): Unit =
    val scanner: Scanner    = Scanner(source)
    val tokens: List[Token] = scanner.scanTokens()
    val parser: Parser      = new Parser(tokens)
    val expression: Expr    = parser.parse()
    if hadError then () else Interpreter.interpret(expression)

  def error(line: Int, message: String): Unit = report(line, "", message)

  def report(line: Int, where: String, message: String): Unit =
    System.err.println(
      "[line " + line + "] Error" + where + ": " + message
    )
    hadError = true

  def error(token: Token, message: String): Unit =
    if token.`type` == TokenType.EOF then report(token.line, " at end", message)
    else report(token.line, " at '" + token.lexeme + "'", message)

  def runtimeError(error: RuntimeError): Unit =
    Console.err.println(s"[line ${error.token.line}] ${error.getMessage()}")
    hadRuntimeError = true

end Slox
