package dev.toniogela.lox.ast

trait StmtVisitor[T]:
//   def visitBlockStmt(stmt: Block): T
//   def visitClassStmt(stmt: Clazz): T
  def visitExpressionStmt(stmt: Expression): T
//   def visitFunctionStmt(stmt: Function): T
//   def visitIfStmt(stmt: If): T
  def visitPrintStmt(stmt: Print): T
//   def visitReturnStmt(stmt: Return): T
//   def visitVarStmt(stmt: Var): T
//   def visitWhileStmt(stmt: While): T

sealed trait Stmt:
  def accept[T](visitor: StmtVisitor[T]): T

case class Expression(expr: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitExpressionStmt(this)

case class Print(expr: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitPrintStmt(this)
