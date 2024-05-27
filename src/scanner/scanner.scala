package dev.toniogela.lox.scanner

import dev.toniogela.lox.Slox

// ! TODO Why separating type and impl? Couldn't we create an ADT?
enum TokenType:

  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,
    // Literals.
    IDENTIFIER, STRING, NUMBER,
    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
    EOF

end TokenType

case class Token(`type`: TokenType, lexeme: String, literal: Any, line: Int):
  override def toString(): String = s"${`type`} $lexeme $literal"

enum Keyword(val name: String, val `type`: TokenType):
  case AND    extends Keyword("and", TokenType.AND)
  case CLASS  extends Keyword("class", TokenType.CLASS)
  case ELSE   extends Keyword("else", TokenType.ELSE)
  case FALSE  extends Keyword("false", TokenType.FALSE)
  case FOR    extends Keyword("for", TokenType.FOR)
  case FUN    extends Keyword("fun", TokenType.FUN)
  case IF     extends Keyword("if", TokenType.IF)
  case NIL    extends Keyword("nil", TokenType.NIL)
  case OR     extends Keyword("or", TokenType.OR)
  case PRINT  extends Keyword("print", TokenType.PRINT)
  case RETURN extends Keyword("return", TokenType.RETURN)
  case SUPER  extends Keyword("super", TokenType.SUPER)
  case THIS   extends Keyword("this", TokenType.THIS)
  case TRUE   extends Keyword("true", TokenType.TRUE)
  case VAR    extends Keyword("var", TokenType.VAR)
  case WHILE  extends Keyword("while", TokenType.WHILE)

end Keyword

class Scanner(val source: String):
  import TokenType.*

  var tokens: List[Token] = Nil // ! TODO use Vector or Chain
  var start: Int          = 0
  var current: Int        = 0
  var line: Int           = 1

  def scanTokens(): List[Token] =
    while !isAtEnd() do
      start = current
      scanToken()

    tokens :+ Token(EOF, "", null, line)

  def isAtEnd(): Boolean = current >= source.length

  def scanToken(): Unit =
    val c: Char = advance()
    c match
      case '('               => addToken(LEFT_PAREN)
      case ')'               => addToken(RIGHT_PAREN)
      case '{'               => addToken(LEFT_BRACE)
      case '}'               => addToken(RIGHT_BRACE)
      case ','               => addToken(COMMA)
      case '.'               => addToken(DOT)
      case '-'               => addToken(MINUS)
      case '+'               => addToken(PLUS)
      case ';'               => addToken(SEMICOLON)
      case '*'               => addToken(STAR)
      case '!'               => addToken(if matches('=') then BANG_EQUAL else BANG)
      case '='               => addToken(if matches('=') then EQUAL_EQUAL else EQUAL)
      case '<'               => addToken(if matches('=') then LESS_EQUAL else LESS)
      case '>'               => addToken(if matches('=') then GREATER_EQUAL else GREATER)
      case '/'               =>
        if matches('/') then
          while peek() != '\n' && !isAtEnd() do advance()
        else addToken(SLASH)
      case ' ' | '\r' | '\t' => () // Ignoring whitespace
      case '\n'              => line = line + 1
      case '"'               => string()

      case c =>
        if isDigit(c) then number()
        else if isAlpha(c) then identifier()
        else Slox.error(line, s"Unexpected character: $c.")

    end match

  end scanToken

  def advance(): Char =
    val value = source.charAt(current)
    current = current + 1
    value

  // Like advance but it doesn't consume
  def peek(): Char =
    if isAtEnd() then '\u0000'
    else source.charAt(current)

  def addToken(`type`: TokenType): Unit = addToken(`type`, null)

  def addToken(`type`: TokenType, literal: Any): Unit =
    val text: String = source.substring(start, current)
    tokens = tokens.appended(new Token(`type`, text, literal, line))

  def matches(expected: Char): Boolean =
    val value = !isAtEnd() && source.charAt(current) == expected
    if value then current = current + 1 else ()
    value

  def string(): Unit =
    while peek() != '"' && !isAtEnd() do {
      if peek() == '\n' then line = line + 1 else ()
      advance()
    }

    if isAtEnd() then Slox.error(line, "Unterminated string.")
    else
      advance() // The closing '"'
      val value: String = source.substring(start + 1, current - 1)
      addToken(STRING, value)

  def isDigit(c: Char): Boolean = c >= 48 && c <= 57 // Byte representation of 0 and 9

  def number(): Unit =
    while isDigit(peek()) do advance()

    if peek() == '.' && isDigit(peekNext()) then {
      // Consume the '.'
      advance()
      while isDigit(peek()) do advance()
    }

    addToken(NUMBER, source.substring(start, current).toDouble)

  def identifier(): Unit =
    while isAlphaNumeric(peek()) do advance()
    val text: String = source.substring(start, current)
    addToken(
      Keyword.values.find(_.name == text).fold(IDENTIFIER)(_.`type`)
    )

  def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

  def peekNext(): Char =
    if current + 1 >= source.length() then '\u0000'
    else source.charAt(current + 1)

end Scanner
