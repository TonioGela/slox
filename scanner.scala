package dev.toniogela.lox

//! TODO Why separating type and impl? Couldn't we create an ADT?
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
    override def toString(): String = s"${`type`} ${lexeme} ${literal}"


enum Identifier(val name:String, val `type`: TokenType):
  case AND extends Identifier("and", TokenType.AND)
  case CLASS extends Identifier("class", TokenType.CLASS)
  case ELSE extends Identifier("else", TokenType.ELSE)
  case FALSE extends Identifier("false", TokenType.FALSE)
  case FOR extends Identifier("for", TokenType.FOR)
  case FUN extends Identifier("fun", TokenType.FUN)
  case IF extends Identifier("if", TokenType.IF)
  case NIL extends Identifier("nil", TokenType.NIL)
  case OR extends Identifier("or", TokenType.OR)
  case PRINT extends Identifier("print", TokenType.PRINT)
  case RETURN extends Identifier("return", TokenType.RETURN)
  case SUPER extends Identifier("super", TokenType.SUPER)
  case THIS extends Identifier("this", TokenType.THIS)
  case TRUE extends Identifier("true", TokenType.TRUE)
  case VAR extends Identifier("var", TokenType.VAR)
  case WHILE extends Identifier("while", TokenType.WHILE)

class Scanner(val source: String):
    import TokenType.*

    var tokens: List[Token] = Nil // TODO use Vector or Chain
    var start: Int = 0
    var current: Int = 0
    var line :Int = 1

    def scanTokens(): List[Token] =
        while(!isAtEnd()) do
            start = current
            scanToken()
        
        tokens :+ Token(EOF, "", null, line)

    def isAtEnd(): Boolean = current >= source.length

    def scanToken(): Unit =
        val c: Char = advance()
        c match 
          case '(' => addToken(LEFT_PAREN)
          case ')' => addToken(RIGHT_PAREN)
          case '{' => addToken(LEFT_BRACE)
          case '}' => addToken(RIGHT_BRACE)
          case ',' => addToken(COMMA)
          case '.' => addToken(DOT)
          case '-' => addToken(MINUS)
          case '+' => addToken(PLUS)
          case ';' => addToken(SEMICOLON)
          case '*' => addToken(STAR)
          case '!' => addToken(if matches('=') then BANG_EQUAL else BANG)
          case '=' => addToken(if matches('=') then EQUAL_EQUAL else EQUAL)
          case '<' => addToken(if matches('=') then LESS_EQUAL else LESS)
          case '>' => addToken(if matches('=') then GREATER_EQUAL else GREATER)
          case '/' =>
            if matches('/') then
              while (peek() != '\n' && !isAtEnd()) do advance()
            else addToken(SLASH)
          case ' ' | '\r' | '\t' => () //Ignoring whitespace
          case '\n' => line = line + 1
          case '"' => string()
          
          case c =>
            if isDigit(c) then number() else 
            if isAlpha(c) then identifier() 
            else Slox.error(line, s"Unexpected character: $c.")
    end scanToken

    def advance() : Char =
        val value = source.charAt(current)
        current = current + 1
        value

    // Like advance but it doesn't consume
    def peek(): Char =
      if isAtEnd() then '\u0000'
      else source.charAt(current)

    def addToken(`type`: TokenType): Unit =
      addToken(`type`, null)
    
    def addToken(`type`: TokenType, literal: Any): Unit =
      val text: String = source.substring(start, current)
      tokens = tokens.appended(new Token(`type`, text, literal, line))

    def matches(expected: Char): Boolean =
      val value = !isAtEnd() && source.charAt(current) == expected
      if value then current = current + 1 else ()
      value

    def string(): Unit =
      while (peek() != '"' && !isAtEnd()) do {
        if (peek() == '\n') then line = line + 1 else ()
        advance()
      }

      if(isAtEnd()) then Slox.error(line, "Unterminated string.")
      else
        advance() // The closing '"'
        val value: String = source.substring(start + 1, current - 1)
        addToken(STRING, value)
    end string

    def isDigit(c: Char): Boolean = c >= 0 && c <= 9

    def number(): Unit =
      while (isDigit(peek())) do advance()

      if (peek() == '.' && isDigit(peekNext())) then {
        // Consume the '.'
        advance()
        while (isDigit(peek())) do advance()
      }

      addToken(NUMBER, source.substring(start, current).toDouble)
    end number

    def identifier(): Unit =
      while isAlphaNumeric(peek()) do advance()
      val text: String = source.substring(start, current)
      addToken(
        Identifier.values.find(_.name == text).fold(IDENTIFIER)(_.`type`)
      )

    def isAlpha(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

    def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

    def peekNext(): Char =
      if (current + 1 >= source.length()) then '\u0000'
      else source.charAt(current + 1)

end Scanner
