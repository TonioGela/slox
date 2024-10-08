package dev.toniogela.lox.ast

import dev.toniogela.lox.scanner.Token

trait StmtVisitor[T]:
  def visitBlockStmt(stmt: Stmt.Block): T
  // def visitClassStmt(stmt: Clazz): T
  def visitExpressionStmt(stmt: Stmt.Expression): T
  def visitFunctionStmt(stmt: Stmt.Function): T
  def visitIfStmt(stmt: Stmt.If): T
  def visitPrintStmt(stmt: Stmt.Print): T
  // def visitReturnStmt(stmt: Stmt.Return): T
  def visitVarStmt(stmt: Stmt.Var): T
  def visitWhileStmt(stmt: Stmt.While): T

sealed trait Stmt:
  def accept[T](visitor: StmtVisitor[T]): T

object Stmt:

  case class Block(statements: List[Stmt]) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitBlockStmt(this)

  // case class Clazz(name: Token, superclass: Variable, methods: List[Function]) extends Stmt:
  //   override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitClassStmt(this)

  case class Expression(expr: Expr) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitExpressionStmt(this)

  case class Function(name: Token, params: List[Token], body: List[Stmt]) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitFunctionStmt(this)

  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitIfStmt(this)

  case class Print(expr: Expr) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitPrintStmt(this)

  // case class Return(keyword: Token, value: Expr) extends Stmt:
  //   override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitReturnStmt(this)

  // * initializer is null when the variable is not initialised. We can replace it with an option
  case class Var(name: Token, initializer: Expr) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitVarStmt(this)

  case class While(condition: Expr, body: Stmt) extends Stmt:
    override def accept[T](visitor: StmtVisitor[T]): T = visitor.visitWhileStmt(this)

end Stmt
