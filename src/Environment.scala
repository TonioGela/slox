package dev.toniogela.lox

import scala.collection.mutable.Map
import dev.toniogela.lox.scanner.Token
import dev.toniogela.lox.ast.Interpreter.RuntimeError

class Environment(enclosing: Environment):
  val values: Map[String, Any] = Map.empty[String, Any]

  def define(name: String, value: Any): Unit = values.addOne(name -> value)

  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then values.update(name.lexeme, value)
    else if enclosing != null then enclosing.assign(name, value)
    else throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.");

  def get(name: Token): Any = values.get(name.lexeme).orElse(
    Option.when(enclosing != null)(enclosing.get(name))
  ).getOrElse(
    throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  )

  def child(): Environment = new Environment(this)

end Environment

object Environment:
  def apply() = new Environment(null)
