package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.*

trait Visitor[T]:
  def visitAssignExpr(expr: Assign): T
  def visitBinaryExpr(expr: Binary): T
  // def visitCallExpr(expr: Call): T
  // def visitGetExpr(expr: Get): T
  def visitGroupingExpr(expr: Grouping): T
  def visitLiteralExpr(expr: Literal): T
  def visitLogicalExpr(expr: Logical): T
  // def visitSetExpr(expr: Set): T
  // def visitSuperExpr(expr: Super): T
  // def visitThisExpr(expr: This): T
  def visitUnaryExpr(expr: Unary): T
  def visitVariableExpr(expr: Variable): T

sealed trait Expr:
  def accept[T](visitor: Visitor[T]): T

case class Assign(name: Token, value: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitAssignExpr(this)

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitBinaryExpr(this)

// case class Call(callee: Expr, paren: Token, arguments: List[Expr]) extends Expr:
//   override def accept[T](visitor: Visitor[T]): T = visitor.visitCallExpr(this)

// case class Get(item: Expr, name: Token) extends Expr:
//   override def accept[T](visitor: Visitor[T]): T = visitor.visitGetExpr(this)

case class Grouping(expression: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitGroupingExpr(this)

case class Literal(value: Any) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitLiteralExpr(this)

case class Logical(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitLogicalExpr(this)

// case class Set(item: Expr, name: Token, value: Expr) extends Expr:
//   override def accept[T](visitor: Visitor[T]): T = visitor.visitSetExpr(this)

// case class Super(keyword: Token, method: Token) extends Expr:
//   override def accept[T](visitor: Visitor[T]): T = visitor.visitSuperExpr(this)

// case class This(keyword: Token) extends Expr:
//   override def accept[T](visitor: Visitor[T]): T = visitor.visitThisExpr(this)

case class Unary(operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitUnaryExpr(this)

case class Variable(name: Token) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visitVariableExpr(this)
