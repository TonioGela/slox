package dev.toniogela.slox

import cats._
import cats.syntax.all._
import cats.effect._
import cats.effect.std.Console
import fs2.io.file.{Files, Path}
import fs2.text.decodeWithCharset
import java.nio.charset.Charset

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = args match {
    case Nil         => runPrompt[IO]
    case path :: Nil => runFile[IO](path)
    case _           => Console[IO].errorln("Usage: slox [script]").as(ExitCode(64))
  }

  def runPrompt[F[_]: Console: MonadThrow]: F[ExitCode] = {
    def read: F[String] = (Console[F].print("> ") >> Console[F].readLine)
      .flatMap { s => if (s.isBlank) read else s.pure[F] }

    val readEvalPrint: F[Unit]         = read >>= execute >>= Console[F].println
    val readEvalPrintLoop: F[ExitCode] = readEvalPrint >> runPrompt

    readEvalPrintLoop.handleError {
      case _: java.io.EOFException => ExitCode.Success // This traps Control-D
      case _                       => ExitCode.Error
    }
  }

  def runFile[F[_]: Async: Console: Files](path: String): F[ExitCode] = {
    val absPath: Path = Path(path).absolute

    Files[F]
      .readAll(absPath)
      .through(decodeWithCharset(Charset.defaultCharset()))
      .evalMap(execute)
      .foreach(Console[F].print)
      .compile
      .drain
      .as(ExitCode.Success)
      // TODO: This error handling should probably be done on file reading and not on execute evaluation
      .handleErrorWith {
        case _: java.nio.file.NoSuchFileException =>
          Console[F].errorln(s"File ${absPath.toString} doesn't exist").as(ExitCode(66))
        case _: java.io.IOException               =>
          Console[F].errorln(s"There was an IO error with the file \"${absPath.toString}\":") >>
            Console[F]
              .errorln(s"Check that the file exists and is not a directory")
              .as(ExitCode(65))
        case x                                    =>
          Console[F].errorln(
            s"There was an unknown error reading the file \"${absPath.toString}\", please report it to the developer:"
          ) >> Console[F]
            .errorln(s"${x.getClass.getCanonicalName()}: ${x.getMessage()}")
            .as(ExitCode(70))
      }
  }

  // TODO: Maybe this buddy should return F[Either[NonEmptyList[CustomErrors], String]]
  def execute[F[_]: Applicative](source: String): F[String] =
    Applicative[F].pure(source)

}
