package dev.toniogela.lox.ast

import dev.toniogela.lox.Environment

trait LoxCallable {
  val arity: Int
  def call(args: List[Any]): Any
}

class LoxFunction(declaration: Stmt.Function) extends LoxCallable {

  override val arity: Int = declaration.params.size

  override def call(args: List[Any]): Any =
    val environment: Environment = new Environment(Interpreter.globals)
    declaration.params.map(_.lexeme).zip(args).foreach(environment.define.tupled)
    Interpreter.executeBlock(declaration.body, environment)

  override def toString(): String = s"<fn ${declaration.name.lexeme} >"
}
