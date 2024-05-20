package dev.toniogela.lox.scanner

import dev.toniogela.lox.scanner.TokenType.*
import munit.*
import scala.collection.immutable.ArraySeq.ofInt

// Testing all the snippets in chapter 3 "The Lox Language"
class ScannerTest extends FunSuite {

  extension (s:String)
    def convertsTo(ts: List[Token])(using Location): Unit = assertEquals(
      new Scanner(s.stripMargin).scanTokens(),
      ts :+ Token(EOF, "", null, s.count(_ == '\n') + 1)
    )

    def convertsTo(t: Token)(using Location): Unit = convertsTo(t :: Nil)

  test("Scanner should recognise simple expressions"){
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
       |print "Hello, world!";""".convertsTo(List(
          Token(PRINT, "print", null, 2),
          Token(STRING, "\"Hello, world!\"", "Hello, world!", 2),
          Token(SEMICOLON, ";", null, 2)
        )
      )

    """|true;  // Not false.
       |false; // Not *not* false.""".convertsTo(List(
          Token(TRUE, "true", null, 1), Token(SEMICOLON, ";", null, 1),
          Token(FALSE, "false", null, 2), Token(SEMICOLON, ";", null, 2)
        )
      )

    """|1234;  // An integer.
       |12.34; // A decimal number.""".convertsTo(List(
          Token(NUMBER, "1234", 1234.0d, 1), Token(SEMICOLON, ";", null, 1),
          Token(NUMBER, "12.34", 12.34d, 2), Token(SEMICOLON, ";", null, 2)
        )
      )

    """|"I am a string";
       |"";    // The empty string.
       |"123"; // This is a string, not a number.""".convertsTo(List(
          Token(STRING, "\"I am a string\"", "I am a string", 1), Token(SEMICOLON, ";", null, 1),
          Token(STRING, "\"\"", "", 2), Token(SEMICOLON, ";", null, 2),
          Token(STRING, "\"123\"", "123", 3), Token(SEMICOLON, ";", null, 3)
        )
       )

    """|add + me;
       |subtract - me;
       |multiply * me;
       |divide / me;""".convertsTo(List(
        Token(IDENTIFIER, "add", null, 1), Token(PLUS, "+", null, 1), Token(IDENTIFIER, "me", null, 1), Token(SEMICOLON, ";", null, 1),
        Token(IDENTIFIER, "subtract", null, 2), Token(MINUS, "-", null, 2), Token(IDENTIFIER, "me", null, 2),Token(SEMICOLON, ";", null, 2),
        Token(IDENTIFIER, "multiply", null, 3), Token(STAR, "*", null, 3), Token(IDENTIFIER, "me", null, 3), Token(SEMICOLON, ";", null, 3),
        Token(IDENTIFIER, "divide", null, 4), Token(SLASH, "/", null, 4), Token(IDENTIFIER, "me", null, 4), Token(SEMICOLON, ";", null, 4)
       ))

    "-negateMe;".convertsTo(List(
      Token(MINUS, "-", null, 1), Token(IDENTIFIER, "negateMe", null, 1), Token(SEMICOLON, ";", null, 1)
    ))

    """|less < than;
       |lessThan <= orEqual;
       |greater > than;
       |greaterThan >= orEqual;""".convertsTo(Nil)

    """|1 == 2;         // false.
       |"cat" != "dog"; // true.""".convertsTo(Nil)

    """314 == "pi"; // false.""".convertsTo(Nil)

    """123 == "123"; // false.""".convertsTo(Nil)

    """|!true;  // false.
       |!false; // true.""".convertsTo(Nil)

    """|true and false; // false.
       |true and true;  // true.""".convertsTo(Nil)

    """|false or false; // false.
       |true or false;  // true.""".convertsTo(Nil)

    "var average = (min + max) / 2;".convertsTo(Nil)

    """print "Hello, world!";""".convertsTo(Nil)

    """"some expression";""".convertsTo(Nil)

    """|{
       |  print "One statement.";
       |  print "Two statements.";
       |}""".convertsTo(Nil)

    """|var imAVariable = "here is my value";
       |var iAmNil;""".convertsTo(Nil)

    """|var breakfast = "bagels";
       |print breakfast; // "bagels".
       |breakfast = "beignets";
       |print breakfast; // "beignets".""".convertsTo(Nil)

    """|if (condition) {
       |  print "yes";
       |} else {
       |  print "no";
       |}""".convertsTo(Nil)

    """|var a = 1;
       |while (a < 10) {
       |  print a;
       |  a = a + 1;
       |}""".convertsTo(Nil)

    """|for (var a = 1; a < 10; a = a + 1) {
       |  print a;
       |}""".convertsTo(Nil)

  }

  test("Scanner should recognise function expressions"){
    "makeBreakfast(bacon, eggs, toast);".convertsTo(Nil)

    "makeBreakfast();".convertsTo(Nil)

    """|fun printSum(a, b) {
       | print a + b;
       |}""".convertsTo(Nil)

    """|fun returnSum(a, b) {
       |  return a + b;
       |}""".convertsTo(Nil)

    """|fun addPair(a, b) {
       |  return a + b;
       |}
       |
       |fun identity(a) {
       |  return a;
       |}
       |
       |print identity(addPair)(1, 2); // Prints "3"""".convertsTo(Nil)

    """|fun outerFunction() {
       |  fun localFunction() {
       |    print "I'm local!";
       |  }
       |
       |  localFunction();
       |}""".convertsTo(Nil)

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
       |fn();""".convertsTo(Nil)
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
       |}""".convertsTo(Nil)

    """|// Store it in variables.
       |var someVariable = Breakfast;
       |// Pass it to functions.
       |someFunction(Breakfast);""".convertsTo(Nil)

    """|var breakfast = Breakfast();
       |print breakfast; // "Breakfast instance".
       |breakfast.meat = "sausage";
       |breakfast.bread = "sourdough";""".convertsTo(Nil)

    """|class Breakfast {
       |  serve(who) {
       |    print "Enjoy your " + this.meat + " and " +
       |        this.bread + ", " + who + ".";
       |  }
       |
       |  // ...
       |} """.convertsTo(Nil)

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
       |// "Enjoy your bacon and toast, Dear Reader."""".convertsTo(Nil)

    """|class Brunch < Breakfast {
       |  drink() {
       |    print "How about a Bloody Mary?";
       |  }
       |}""".convertsTo(Nil)

    """|var benedict = Brunch("ham", "English muffin");
       |benedict.serve("Noble Reader");""".convertsTo(Nil)

    """|class Brunch < Breakfast {
       |  init(meat, bread, drink) {
       |    super.init(meat, bread);
       |    this.drink = drink;
       |  }
       |}""".convertsTo(Nil)
  }

}
