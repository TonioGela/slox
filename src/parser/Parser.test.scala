package dev.toniogela.lox.parser

import munit.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import dev.toniogela.lox.ast.*
import dev.toniogela.lox.scanner.*
import dev.toniogela.lox.scanner.TokenType.*

class ParserTest extends ScalaCheckSuite:

  extension (t: Token)

    def parsesTo(expr: Expr)(using Location): Unit = assertEquals(
      new Parser(t :: Token(EOF, "", null, 1) :: Nil).parse(),
      expr,
    )

  extension (ts: List[Token])

    def parsesTo(expr: Expr)(using Location): Unit = assertEquals(
      new Parser(ts :+ Token(EOF, "", null, ts.map(_.line).maxOption.fold(1)(_ + 1))).parse(),
      expr,
    )

  property("All the strings should be parsed to Literal strings") {
    forAll((s: String) => Token(STRING, s"\"$s\"", s, 1).parsesTo(new Literal(s)))
  }

  property("String concatenation should result in string concatenation") {
    forAll((a: String, b: String) =>
      List(
        Token(STRING, s"\"$a\"", a, 1),
        Token(PLUS, "+", null, 1),
        Token(STRING, s"\"$b\"", b, 1),
      ).parsesTo(
        new Binary(
          new Literal(a),
          Token(PLUS, "+", null, 1),
          new Literal(b),
        )
      )
    )
  }

  property("All the Ints should be parsed to Literal Doubles") {
    forAll((n: Int) => Token(NUMBER, s"$n", n, 1).parsesTo(new Literal(n.toDouble)))
  }

  property("All the Doubles should be parsed to Literal Doubles") {
    forAll((n: Double) => Token(NUMBER, s"$n", n, 1).parsesTo(new Literal(n)))
  }

  val number = Gen.chooseNum[Double](Double.MinValue / 2, Double.MaxValue / 2)

  val operation = Gen.oneOf(("*", STAR), ("-", MINUS), ("+", PLUS), ("/", SLASH))
    .map((l, o) => Token(o, l, null, 1))

  val binaryOp = Gen.zip(number, operation, number)

  property("Number operations should result in number operations") {
    forAll(binaryOp) { (n, o, m) =>
      List(
        Token(NUMBER, n.toString, n, 1),
        o,
        Token(NUMBER, m.toString, m, 1),
      ).parsesTo(new Binary(new Literal(n), o, new Literal(m)))
    }
  }

  property("Grouped number operations should result in grouped number operations") {
    forAll(Gen.zip(binaryOp, operation, binaryOp)) { case ((a, o1, b), o2, (c, o3, d)) =>
      List(
        Token(LEFT_PAREN, "(", null, 1),
        Token(NUMBER, a.toString, a, 1),
        o1,
        Token(NUMBER, b.toString, b, 1),
        Token(RIGHT_PAREN, ")", null, 1),
        o2,
        Token(LEFT_PAREN, "(", null, 1),
        Token(NUMBER, c.toString, c, 1),
        o3,
        Token(NUMBER, d.toString, d, 1),
        Token(RIGHT_PAREN, ")", null, 1),
      ).parsesTo(
        new Binary(
          new Grouping(
            new Binary(
              new Literal(a),
              o1,
              new Literal(b),
            )
          ),
          o2,
          new Grouping(
            new Binary(
              new Literal(c),
              o3,
              new Literal(d),
            )
          ),
        )
      )
    }
  }

  // -a * b - c <= d == e
  property("Operators should respect precedence") {
    forAll(Gen.zip(
      Gen.posNum[Int],
      Gen.posNum[Int],
      Gen.posNum[Int],
      Gen.posNum[Int],
      Gen.posNum[Int],
    ))((a, b, c, d, e) =>
      List(
        Token(MINUS, "-", null, 1),
        Token(NUMBER, a.toString, a, 1),
        Token(STAR, "*", null, 1),
        Token(NUMBER, b.toString, b, 1),
        Token(PLUS, "+", null, 1),
        Token(NUMBER, c.toString, c, 1),
        Token(LESS_EQUAL, "<=", null, 1),
        Token(NUMBER, d.toString, d, 1),
        Token(EQUAL_EQUAL, "==", null, 1),
        Token(NUMBER, e.toString, e, 1),
      ).parsesTo(
        new Binary(
          new Binary(
            new Binary(
              new Binary(
                new Unary(
                  Token(MINUS, "-", null, 1),
                  new Literal(a),
                ),
                Token(STAR, "*", null, 1),
                new Literal(b),
              ),
              Token(PLUS, "+", null, 1),
              new Literal(c),
            ),
            Token(LESS_EQUAL, "<=", null, 1),
            new Literal(d),
          ),
          Token(EQUAL_EQUAL, "==", null, 1),
          new Literal(e),
        )
      )
    )
  }

end ParserTest
