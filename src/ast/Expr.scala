package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*

trait Visitor[T]:
  def visitBinaryExpr(expr: Binary): T
  def visitGroupingExpr(expr: Grouping): T
  def visitLiteralExpr(expr: Literal): T
  def visitUnaryExpr(expr: Unary): T

sealed trait Expr:
  def accept[T](visitor: Visitor[T]): T

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitBinaryExpr(this)

case class Grouping(expression: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitGroupingExpr(this)

case class Literal(value: Any) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitLiteralExpr(this)

case class Unary(operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitUnaryExpr(this)
