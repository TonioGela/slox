package dev.toniogela.lox.ast

trait LoxCallable {
  val arity: Int
  def call(args: List[Any]): Any
}
