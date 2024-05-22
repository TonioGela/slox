package dev.toniogela.lox

import java.io.{BufferedReader, InputStreamReader}
import scala.io.Source
import dev.toniogela.lox.scanner.{Scanner, Token}

object Slox:
  //! TODO Mutability
  var hadError: Boolean = false

  def main(args: Array[String]): Unit = args.toList match
    case Nil => runPrompt()
    case file :: Nil => runFile(file)
    case _ =>
        System.err.println("Usage: slox [script]")
        System.exit(64)

    def runFile(file: String): Unit =
        run(Source.fromFile(file).getLines().mkString("\n"))
        if (hadError) System.exit(65)

    def runPrompt(): Unit =
      def readEval(reader: BufferedReader): Unit =
        print("> ")
        Option(reader.readLine()).fold(println("Bye bye!"))(run)
        readEval(reader)
        hadError = false

      readEval(new BufferedReader(new InputStreamReader(System.in)))

    def run(source: String): Unit = 
      val scanner: Scanner = Scanner(source)
      val tokens: List[Token] = scanner.scanTokens()
      tokens.foreach(println)
    
    def error(line: Int, message: String): Unit =
      report(line, "", message)

    def report(line: Int, where: String, message: String): Unit =
      System.err.println(
        "[line " + line + "] Error" + where + ": " + message
      )
      hadError = true
