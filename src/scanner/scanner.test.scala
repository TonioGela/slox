package dev.toniogela.lox.scanner

import dev.toniogela.lox.scanner.TokenType.*
import munit.*
import scala.collection.immutable.ArraySeq.ofInt
import munit.diff.*
import munit.internal.console.Printers

// Testing all the snippets in chapter 3 "The Lox Language"
class ScannerTest extends FunSuite {

  override def printer: Printer = Printer(200)(PartialFunction.empty)

  extension (s: String)

    def convertsTo(ts: List[Token])(using Location): Unit = assertEquals(
      new Scanner(s.stripMargin).scanTokens(),
      ts :+ Token(EOF, "", null, s.count(_ == '\n') + 1),
    )

    def convertsTo(t: Token)(using Location): Unit   = convertsTo(t :: Nil)
    def convertsTo(ts: Token*)(using Location): Unit = convertsTo(ts.toList)

  test("Scanner should recognise simple expressions") {
    // Single char tokens
    "(".convertsTo(Token(LEFT_PAREN, "(", null, 1))
    ")".convertsTo(Token(RIGHT_PAREN, ")", null, 1))
    "{".convertsTo(Token(LEFT_BRACE, "{", null, 1))
    "}".convertsTo(Token(RIGHT_BRACE, "}", null, 1))
    ",".convertsTo(Token(COMMA, ",", null, 1))
    ".".convertsTo(Token(DOT, ".", null, 1))
    "-".convertsTo(Token(MINUS, "-", null, 1))
    "+".convertsTo(Token(PLUS, "+", null, 1))
    ";".convertsTo(Token(SEMICOLON, ";", null, 1))
    "*".convertsTo(Token(STAR, "*", null, 1))
    "!".convertsTo(Token(BANG, "!", null, 1))
    "!=".convertsTo(Token(BANG_EQUAL, "!=", null, 1))
    "=".convertsTo(Token(EQUAL, "=", null, 1))
    "==".convertsTo(Token(EQUAL_EQUAL, "==", null, 1))
    "<".convertsTo(Token(LESS, "<", null, 1))
    "<=".convertsTo(Token(LESS_EQUAL, "<=", null, 1))
    ">".convertsTo(Token(GREATER, ">", null, 1))
    ">=".convertsTo(Token(GREATER_EQUAL, ">=", null, 1))
    "/".convertsTo(Token(SLASH, "/", null, 1))

    // Comments
    "//".convertsTo(Nil)
    "// paperino".convertsTo(Nil)

    // Identifiers
    "1".convertsTo(Token(NUMBER, "1", 1.0d, 1) :: Nil)
    "\"foo\"".convertsTo(Token(STRING, "\"foo\"", "foo", 1) :: Nil)
    "foo".convertsTo(Token(IDENTIFIER, "foo", null, 1) :: Nil)

    // Keywords
    "and".convertsTo(Token(AND, "and", null, 1))
    "class".convertsTo(Token(CLASS, "class", null, 1))
    "else".convertsTo(Token(ELSE, "else", null, 1))
    "false".convertsTo(Token(FALSE, "false", null, 1))
    "for".convertsTo(Token(FOR, "for", null, 1))
    "fun".convertsTo(Token(FUN, "fun", null, 1))
    "if".convertsTo(Token(IF, "if", null, 1))
    "nil".convertsTo(Token(NIL, "nil", null, 1))
    "or".convertsTo(Token(OR, "or", null, 1))
    "print".convertsTo(Token(PRINT, "print", null, 1))
    "return".convertsTo(Token(RETURN, "return", null, 1))
    "super".convertsTo(Token(SUPER, "super", null, 1))
    "this".convertsTo(Token(THIS, "this", null, 1))
    "true".convertsTo(Token(TRUE, "true", null, 1))
    "var".convertsTo(Token(VAR, "var", null, 1))
    "while".convertsTo(Token(WHILE, "while", null, 1))
  }

  test("Scanner should recognise tiny scripts") {
    """|// Your first Lox program!
       |print "Hello, world!";""".convertsTo(
      Token(PRINT, "print", null, 2),
      Token(STRING, "\"Hello, world!\"", "Hello, world!", 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|true;  // Not false.
       |false; // Not *not* false.""".convertsTo(
      Token(TRUE, "true", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(FALSE, "false", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|1234;  // An integer.
       |12.34; // A decimal number.""".convertsTo(
      Token(NUMBER, "1234", 1234.0d, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(NUMBER, "12.34", 12.34d, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|"I am a string";
       |"";    // The empty string.
       |"123"; // This is a string, not a number.""".convertsTo(
      Token(STRING, "\"I am a string\"", "I am a string", 1),
      Token(SEMICOLON, ";", null, 1),
      Token(STRING, "\"\"", "", 2),
      Token(SEMICOLON, ";", null, 2),
      Token(STRING, "\"123\"", "123", 3),
      Token(SEMICOLON, ";", null, 3),
    )

    """|add + me;
       |subtract - me;
       |multiply * me;
       |divide / me;""".convertsTo(
      Token(IDENTIFIER, "add", null, 1),
      Token(PLUS, "+", null, 1),
      Token(IDENTIFIER, "me", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(IDENTIFIER, "subtract", null, 2),
      Token(MINUS, "-", null, 2),
      Token(IDENTIFIER, "me", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(IDENTIFIER, "multiply", null, 3),
      Token(STAR, "*", null, 3),
      Token(IDENTIFIER, "me", null, 3),
      Token(SEMICOLON, ";", null, 3),
      Token(IDENTIFIER, "divide", null, 4),
      Token(SLASH, "/", null, 4),
      Token(IDENTIFIER, "me", null, 4),
      Token(SEMICOLON, ";", null, 4),
    )

    "-negateMe;".convertsTo(
      Token(MINUS, "-", null, 1),
      Token(IDENTIFIER, "negateMe", null, 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """|less < than;
       |lessThan <= orEqual;
       |greater > than;
       |greaterThan >= orEqual;""".convertsTo(
      Token(IDENTIFIER, "less", null, 1),
      Token(LESS, "<", null, 1),
      Token(IDENTIFIER, "than", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(IDENTIFIER, "lessThan", null, 2),
      Token(LESS_EQUAL, "<=", null, 2),
      Token(IDENTIFIER, "orEqual", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(IDENTIFIER, "greater", null, 3),
      Token(GREATER, ">", null, 3),
      Token(IDENTIFIER, "than", null, 3),
      Token(SEMICOLON, ";", null, 3),
      Token(IDENTIFIER, "greaterThan", null, 4),
      Token(GREATER_EQUAL, ">=", null, 4),
      Token(IDENTIFIER, "orEqual", null, 4),
      Token(SEMICOLON, ";", null, 4),
    )

    """|1 == 2;         // false.
       |"cat" != "dog"; // true.""".convertsTo(
      Token(NUMBER, "1", 1, 1),
      Token(EQUAL_EQUAL, "==", null, 1),
      Token(NUMBER, "2", 2, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(STRING, "\"cat\"", "cat", 2),
      Token(BANG_EQUAL, "!=", null, 2),
      Token(STRING, "\"dog\"", "dog", 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """314 == "pi"; // false.""".convertsTo(
      Token(NUMBER, "314", 314d, 1),
      Token(EQUAL_EQUAL, "==", null, 1),
      Token(STRING, "\"pi\"", "pi", 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """123 == "123"; // false.""".convertsTo(
      Token(NUMBER, "123", 123d, 1),
      Token(EQUAL_EQUAL, "==", null, 1),
      Token(STRING, "\"123\"", "123", 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """|!true;  // false.
       |!false; // true.""".convertsTo(
      Token(BANG, "!", null, 1),
      Token(TRUE, "true", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(BANG, "!", null, 2),
      Token(FALSE, "false", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|true and false; // false.
       |true and true;  // true.""".convertsTo(
      Token(TRUE, "true", null, 1),
      Token(AND, "and", null, 1),
      Token(FALSE, "false", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(TRUE, "true", null, 2),
      Token(AND, "and", null, 2),
      Token(TRUE, "true", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|false or false; // false.
       |true or false;  // true.""".convertsTo(
      Token(FALSE, "false", null, 1),
      Token(OR, "or", null, 1),
      Token(FALSE, "false", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(TRUE, "true", null, 2),
      Token(OR, "or", null, 2),
      Token(FALSE, "false", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    "var average = (min + max) / 2;".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "average", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "min", null, 1),
      Token(PLUS, "+", null, 1),
      Token(IDENTIFIER, "max", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(SLASH, "/", null, 1),
      Token(NUMBER, "2", 2d, 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """print "Hello, world!";""".convertsTo(
      Token(PRINT, "print", null, 1),
      Token(STRING, "\"Hello, world!\"", "Hello, world!", 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """"some expression";""".convertsTo(
      Token(
        STRING,
        "\"some expression\"",
        "some expression",
        1,
      ),
      Token(SEMICOLON, ";", null, 1),
    )

    """|{
       |  print "One statement.";
       |  print "Two statements.";
       |}""".convertsTo(
      Token(LEFT_BRACE, "{", null, 1),
      Token(PRINT, "print", null, 2),
      Token(STRING, "\"One statement.\"", "One statement.", 2),
      Token(SEMICOLON, ";", null, 2),
      Token(PRINT, "print", null, 3),
      Token(STRING, "\"Two statements.\"", "Two statements.", 3),
      Token(SEMICOLON, ";", null, 3),
      Token(RIGHT_BRACE, "}", null, 4),
    )

    """|var imAVariable = "here is my value";
       |var iAmNil;""".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "imAVariable", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(
        STRING,
        "\"here is my value\"",
        "here is my value",
        1,
      ),
      Token(SEMICOLON, ";", null, 1),
      Token(VAR, "var", null, 2),
      Token(IDENTIFIER, "iAmNil", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|var breakfast = "bagels";
       |print breakfast; // "bagels".
       |breakfast = "beignets";
       |print breakfast; // "beignets".""".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "breakfast", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(
        STRING,
        "\"bagels\"",
        "bagels",
        1,
      ),
      Token(SEMICOLON, ";", null, 1),
      Token(PRINT, "print", null, 2),
      Token(IDENTIFIER, "breakfast", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(IDENTIFIER, "breakfast", null, 3),
      Token(EQUAL, "=", null, 3),
      Token(
        STRING,
        "\"beignets\"",
        "beignets",
        3,
      ),
      Token(SEMICOLON, ";", null, 3),
      Token(PRINT, "print", null, 4),
      Token(IDENTIFIER, "breakfast", null, 4),
      Token(SEMICOLON, ";", null, 4),
    )

    """|if (condition) {
       |  print "yes";
       |} else {
       |  print "no";
       |}""".convertsTo(
      Token(IF, "if", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "condition", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(PRINT, "print", null, 2),
      Token(STRING, "\"yes\"", "yes", 2),
      Token(SEMICOLON, ";", null, 2),
      Token(RIGHT_BRACE, "}", null, 3),
      Token(ELSE, "else", null, 3),
      Token(LEFT_BRACE, "{", null, 3),
      Token(PRINT, "print", null, 4),
      Token(STRING, "\"no\"", "no", 4),
      Token(SEMICOLON, ";", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
    )

    """|var a = 1;
       |while (a < 10) {
       |  print a;
       |  a = a + 1;
       |}""".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(NUMBER, "1", 1d, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(WHILE, "while", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(IDENTIFIER, "a", null, 2),
      Token(LESS, "<", null, 2),
      Token(NUMBER, "10", 10d, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(PRINT, "print", null, 3),
      Token(IDENTIFIER, "a", null, 3),
      Token(SEMICOLON, ";", null, 3),
      Token(IDENTIFIER, "a", null, 4),
      Token(EQUAL, "=", null, 4),
      Token(IDENTIFIER, "a", null, 4),
      Token(PLUS, "+", null, 4),
      Token(NUMBER, "1", 1d, 4),
      Token(SEMICOLON, ";", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
    )

    """|for (var a = 1; a < 10; a = a + 1) {
       |  print a;
       |}""".convertsTo(
      Token(FOR, "for", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(NUMBER, "1", 1d, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(LESS, "<", null, 1),
      Token(NUMBER, "10", 10d, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(PLUS, "+", null, 1),
      Token(NUMBER, "1", 1d, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(PRINT, "print", null, 2),
      Token(IDENTIFIER, "a", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(RIGHT_BRACE, "}", null, 3),
    )

  }

  test("Scanner should recognise function expressions") {
    "makeBreakfast(bacon, eggs, toast);".convertsTo(
      Token(IDENTIFIER, "makeBreakfast", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "bacon", null, 1),
      Token(COMMA, ",", null, 1),
      Token(IDENTIFIER, "eggs", null, 1),
      Token(COMMA, ",", null, 1),
      Token(IDENTIFIER, "toast", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(SEMICOLON, ";", null, 1),
    )

    "makeBreakfast();".convertsTo(
      Token(IDENTIFIER, "makeBreakfast", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(SEMICOLON, ";", null, 1),
    )

    """|fun printSum(a, b) {
       | print a + b;
       |}""".convertsTo(
      Token(FUN, "fun", null, 1),
      Token(IDENTIFIER, "printSum", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(COMMA, ",", null, 1),
      Token(IDENTIFIER, "b", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(PRINT, "print", null, 2),
      Token(IDENTIFIER, "a", null, 2),
      Token(PLUS, "+", null, 2),
      Token(IDENTIFIER, "b", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(RIGHT_BRACE, "}", null, 3),
    )

    """|fun returnSum(a, b) {
       |  return a + b;
       |}""".convertsTo(
      Token(FUN, "fun", null, 1),
      Token(IDENTIFIER, "returnSum", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(COMMA, ",", null, 1),
      Token(IDENTIFIER, "b", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(RETURN, "return", null, 2),
      Token(IDENTIFIER, "a", null, 2),
      Token(PLUS, "+", null, 2),
      Token(IDENTIFIER, "b", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(RIGHT_BRACE, "}", null, 3),
    )

    """|fun addPair(a, b) {
       |  return a + b;
       |}
       |
       |fun identity(a) {
       |  return a;
       |}
       |
       |print identity(addPair)(1, 2); // Prints "3"""".convertsTo(
      Token(FUN, "fun", null, 1),
      Token(IDENTIFIER, "addPair", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(IDENTIFIER, "a", null, 1),
      Token(COMMA, ",", null, 1),
      Token(IDENTIFIER, "b", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(RETURN, "return", null, 2),
      Token(IDENTIFIER, "a", null, 2),
      Token(PLUS, "+", null, 2),
      Token(IDENTIFIER, "b", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(RIGHT_BRACE, "}", null, 3),
      Token(FUN, "fun", null, 5),
      Token(IDENTIFIER, "identity", null, 5),
      Token(LEFT_PAREN, "(", null, 5),
      Token(IDENTIFIER, "a", null, 5),
      Token(RIGHT_PAREN, ")", null, 5),
      Token(LEFT_BRACE, "{", null, 5),
      Token(RETURN, "return", null, 6),
      Token(IDENTIFIER, "a", null, 6),
      Token(SEMICOLON, ";", null, 6),
      Token(RIGHT_BRACE, "}", null, 7),
      Token(PRINT, "print", null, 9),
      Token(IDENTIFIER, "identity", null, 9),
      Token(LEFT_PAREN, "(", null, 9),
      Token(IDENTIFIER, "addPair", null, 9),
      Token(RIGHT_PAREN, ")", null, 9),
      Token(LEFT_PAREN, "(", null, 9),
      Token(NUMBER, "1", 1d, 9),
      Token(COMMA, ",", null, 9),
      Token(NUMBER, "2", 2d, 9),
      Token(RIGHT_PAREN, ")", null, 9),
      Token(SEMICOLON, ";", null, 9),
    )

    """|fun outerFunction() {
       |  fun localFunction() {
       |    print "I'm local!";
       |  }
       |
       |  localFunction();
       |}""".convertsTo(
      Token(FUN, "fun", null, 1),
      Token(IDENTIFIER, "outerFunction", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(FUN, "fun", null, 2),
      Token(IDENTIFIER, "localFunction", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(PRINT, "print", null, 3),
      Token(STRING, "\"I'm local!\"", "I'm local!", 3),
      Token(SEMICOLON, ";", null, 3),
      Token(RIGHT_BRACE, "}", null, 4),
      Token(IDENTIFIER, "localFunction", null, 6),
      Token(LEFT_PAREN, "(", null, 6),
      Token(RIGHT_PAREN, ")", null, 6),
      Token(SEMICOLON, ";", null, 6),
      Token(RIGHT_BRACE, "}", null, 7),
    )

    """|fun returnFunction() {
       |  var outside = "outside";
       |
       |  fun inner() {
       |    print outside;
       |  }
       |
       |  return inner;
       |}
       |
       |var fn = returnFunction();
       |fn();""".convertsTo(
      Token(FUN, "fun", null, 1),
      Token(IDENTIFIER, "returnFunction", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(VAR, "var", null, 2),
      Token(IDENTIFIER, "outside", null, 2),
      Token(EQUAL, "=", null, 2),
      Token(STRING, "\"outside\"", "outside", 2),
      Token(SEMICOLON, ";", null, 2),
      Token(FUN, "fun", null, 4),
      Token(IDENTIFIER, "inner", null, 4),
      Token(LEFT_PAREN, "(", null, 4),
      Token(RIGHT_PAREN, ")", null, 4),
      Token(LEFT_BRACE, "{", null, 4),
      Token(PRINT, "print", null, 5),
      Token(IDENTIFIER, "outside", null, 5),
      Token(SEMICOLON, ";", null, 5),
      Token(RIGHT_BRACE, "}", null, 6),
      Token(RETURN, "return", null, 8),
      Token(IDENTIFIER, "inner", null, 8),
      Token(SEMICOLON, ";", null, 8),
      Token(RIGHT_BRACE, "}", null, 9),
      Token(VAR, "var", null, 11),
      Token(IDENTIFIER, "fn", null, 11),
      Token(EQUAL, "=", null, 11),
      Token(IDENTIFIER, "returnFunction", null, 11),
      Token(LEFT_PAREN, "(", null, 11),
      Token(RIGHT_PAREN, ")", null, 11),
      Token(SEMICOLON, ";", null, 11),
      Token(IDENTIFIER, "fn", null, 12),
      Token(LEFT_PAREN, "(", null, 12),
      Token(RIGHT_PAREN, ")", null, 12),
      Token(SEMICOLON, ";", null, 12),
    )
  }

  test("Scanner should recognise inheritance expressions") {
    """|class Breakfast {
       |  cook() {
       |    print "Eggs a-fryin'!";
       |  }
       |
       |  serve(who) {
       |    print "Enjoy your breakfast, " + who + ".";
       |  }
       |}""".convertsTo(
      Token(CLASS, "class", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(IDENTIFIER, "cook", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(PRINT, "print", null, 3),
      Token(STRING, "\"Eggs a-fryin'!\"", "Eggs a-fryin'!", 3),
      Token(SEMICOLON, ";", null, 3),
      Token(RIGHT_BRACE, "}", null, 4),
      Token(IDENTIFIER, "serve", null, 6),
      Token(LEFT_PAREN, "(", null, 6),
      Token(IDENTIFIER, "who", null, 6),
      Token(RIGHT_PAREN, ")", null, 6),
      Token(LEFT_BRACE, "{", null, 6),
      Token(PRINT, "print", null, 7),
      Token(
        STRING,
        "\"Enjoy your breakfast, \"",
        "Enjoy your breakfast, ",
        7,
      ),
      Token(PLUS, "+", null, 7),
      Token(IDENTIFIER, "who", null, 7),
      Token(PLUS, "+", null, 7),
      Token(STRING, "\".\"", ".", 7),
      Token(SEMICOLON, ";", null, 7),
      Token(RIGHT_BRACE, "}", null, 8),
      Token(RIGHT_BRACE, "}", null, 9),
    )

    """|// Store it in variables.
       |var someVariable = Breakfast;
       |// Pass it to functions.
       |someFunction(Breakfast);""".convertsTo(
      Token(VAR, "var", null, 2),
      Token(IDENTIFIER, "someVariable", null, 2),
      Token(EQUAL, "=", null, 2),
      Token(IDENTIFIER, "Breakfast", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(IDENTIFIER, "someFunction", null, 4),
      Token(LEFT_PAREN, "(", null, 4),
      Token(IDENTIFIER, "Breakfast", null, 4),
      Token(RIGHT_PAREN, ")", null, 4),
      Token(SEMICOLON, ";", null, 4),
    )

    """|var breakfast = Breakfast();
       |print breakfast; // "Breakfast instance".
       |breakfast.meat = "sausage";
       |breakfast.bread = "sourdough";""".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "breakfast", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(PRINT, "print", null, 2),
      Token(IDENTIFIER, "breakfast", null, 2),
      Token(SEMICOLON, ";", null, 2),
      Token(IDENTIFIER, "breakfast", null, 3),
      Token(DOT, ".", null, 3),
      Token(IDENTIFIER, "meat", null, 3),
      Token(EQUAL, "=", null, 3),
      Token(STRING, "\"sausage\"", "sausage", 3),
      Token(SEMICOLON, ";", null, 3),
      Token(IDENTIFIER, "breakfast", null, 4),
      Token(DOT, ".", null, 4),
      Token(IDENTIFIER, "bread", null, 4),
      Token(EQUAL, "=", null, 4),
      Token(STRING, "\"sourdough\"", "sourdough", 4),
      Token(SEMICOLON, ";", null, 4),
    )

    """|class Breakfast {
       |  serve(who) {
       |    print "Enjoy your " + this.meat + " and " +
       |        this.bread + ", " + who + ".";
       |  }
       |
       |  // ...
       |}""".convertsTo(
      Token(CLASS, "class", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(IDENTIFIER, "serve", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(IDENTIFIER, "who", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(PRINT, "print", null, 3),
      Token(STRING, "\"Enjoy your \"", "Enjoy your ", 3),
      Token(PLUS, "+", null, 3),
      Token(THIS, "this", null, 3),
      Token(DOT, ".", null, 3),
      Token(IDENTIFIER, "meat", null, 3),
      Token(PLUS, "+", null, 3),
      Token(STRING, "\" and \"", " and ", 3),
      Token(PLUS, "+", null, 3),
      Token(THIS, "this", null, 4),
      Token(DOT, ".", null, 4),
      Token(IDENTIFIER, "bread", null, 4),
      Token(PLUS, "+", null, 4),
      Token(STRING, "\", \"", ", ", 4),
      Token(PLUS, "+", null, 4),
      Token(IDENTIFIER, "who", null, 4),
      Token(PLUS, "+", null, 4),
      Token(STRING, "\".\"", ".", 4),
      Token(SEMICOLON, ";", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
      Token(RIGHT_BRACE, "}", null, 8),
    )

    """|class Breakfast {
       |  init(meat, bread) {
       |    this.meat = meat;
       |    this.bread = bread;
       |  }
       |
       |  // ...
       |}
       |
       |var baconAndToast = Breakfast("bacon", "toast");
       |baconAndToast.serve("Dear Reader");
       |// "Enjoy your bacon and toast, Dear Reader."""".convertsTo(
      Token(CLASS, "class", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(IDENTIFIER, "init", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(IDENTIFIER, "meat", null, 2),
      Token(COMMA, ",", null, 2),
      Token(IDENTIFIER, "bread", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(THIS, "this", null, 3),
      Token(DOT, ".", null, 3),
      Token(IDENTIFIER, "meat", null, 3),
      Token(EQUAL, "=", null, 3),
      Token(IDENTIFIER, "meat", null, 3),
      Token(SEMICOLON, ";", null, 3),
      Token(THIS, "this", null, 4),
      Token(DOT, ".", null, 4),
      Token(IDENTIFIER, "bread", null, 4),
      Token(EQUAL, "=", null, 4),
      Token(IDENTIFIER, "bread", null, 4),
      Token(SEMICOLON, ";", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
      Token(RIGHT_BRACE, "}", null, 8),
      Token(VAR, "var", null, 10),
      Token(IDENTIFIER, "baconAndToast", null, 10),
      Token(EQUAL, "=", null, 10),
      Token(IDENTIFIER, "Breakfast", null, 10),
      Token(LEFT_PAREN, "(", null, 10),
      Token(STRING, "\"bacon\"", "bacon", 10),
      Token(COMMA, ",", null, 10),
      Token(STRING, "\"toast\"", "toast", 10),
      Token(RIGHT_PAREN, ")", null, 10),
      Token(SEMICOLON, ";", null, 10),
      Token(IDENTIFIER, "baconAndToast", null, 11),
      Token(DOT, ".", null, 11),
      Token(IDENTIFIER, "serve", null, 11),
      Token(LEFT_PAREN, "(", null, 11),
      Token(STRING, "\"Dear Reader\"", "Dear Reader", 11),
      Token(RIGHT_PAREN, ")", null, 11),
      Token(SEMICOLON, ";", null, 11),
    )

    """|class Brunch < Breakfast {
       |  drink() {
       |    print "How about a Bloody Mary?";
       |  }
       |}""".convertsTo(
      Token(CLASS, "class", null, 1),
      Token(IDENTIFIER, "Brunch", null, 1),
      Token(LESS, "<", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(IDENTIFIER, "drink", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(PRINT, "print", null, 3),
      Token(
        STRING,
        "\"How about a Bloody Mary?\"",
        "How about a Bloody Mary?",
        3,
      ),
      Token(SEMICOLON, ";", null, 3),
      Token(RIGHT_BRACE, "}", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
    )

    """|var benedict = Brunch("ham", "English muffin");
       |benedict.serve("Noble Reader");""".convertsTo(
      Token(VAR, "var", null, 1),
      Token(IDENTIFIER, "benedict", null, 1),
      Token(EQUAL, "=", null, 1),
      Token(IDENTIFIER, "Brunch", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(STRING, "\"ham\"", "ham", 1),
      Token(COMMA, ",", null, 1),
      Token(STRING, "\"English muffin\"", "English muffin", 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(SEMICOLON, ";", null, 1),
      Token(IDENTIFIER, "benedict", null, 2),
      Token(DOT, ".", null, 2),
      Token(IDENTIFIER, "serve", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(STRING, "\"Noble Reader\"", "Noble Reader", 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(SEMICOLON, ";", null, 2),
    )

    """|class Brunch < Breakfast {
       |  init(meat, bread, drink) {
       |    super.init(meat, bread);
       |    this.drink = drink;
       |  }
       |}""".convertsTo(
      Token(CLASS, "class", null, 1),
      Token(IDENTIFIER, "Brunch", null, 1),
      Token(LESS, "<", null, 1),
      Token(IDENTIFIER, "Breakfast", null, 1),
      Token(LEFT_BRACE, "{", null, 1),
      Token(IDENTIFIER, "init", null, 2),
      Token(LEFT_PAREN, "(", null, 2),
      Token(IDENTIFIER, "meat", null, 2),
      Token(COMMA, ",", null, 2),
      Token(IDENTIFIER, "bread", null, 2),
      Token(COMMA, ",", null, 2),
      Token(IDENTIFIER, "drink", null, 2),
      Token(RIGHT_PAREN, ")", null, 2),
      Token(LEFT_BRACE, "{", null, 2),
      Token(SUPER, "super", null, 3),
      Token(DOT, ".", null, 3),
      Token(IDENTIFIER, "init", null, 3),
      Token(LEFT_PAREN, "(", null, 3),
      Token(IDENTIFIER, "meat", null, 3),
      Token(COMMA, ",", null, 3),
      Token(IDENTIFIER, "bread", null, 3),
      Token(RIGHT_PAREN, ")", null, 3),
      Token(SEMICOLON, ";", null, 3),
      Token(THIS, "this", null, 4),
      Token(DOT, ".", null, 4),
      Token(IDENTIFIER, "drink", null, 4),
      Token(EQUAL, "=", null, 4),
      Token(IDENTIFIER, "drink", null, 4),
      Token(SEMICOLON, ";", null, 4),
      Token(RIGHT_BRACE, "}", null, 5),
      Token(RIGHT_BRACE, "}", null, 6),
    )
  }

}
